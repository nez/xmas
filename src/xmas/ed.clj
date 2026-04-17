(ns xmas.ed
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.cmd :as cmd]
            [xmas.el :as el]
            [xmas.gap :as gap]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.view :as view]
            [xmas.log :as log]
            [nrepl.server :as nrepl]
            [xmas.web :as web]
            [xmas.dev :as dev])
  (:import [java.nio.file Files StandardCopyOption])
  (:gen-class))

;; --- Editor state ---

(def editor (atom nil))

;; Thin re-exports — tests reach for ed/cur, ed/forward-char, etc.
;; (Clojure's :refer would bring these into ed's namespace locally, but
;;  external callers like tests need actual vars in ed/, so we `def` them.)
(defmacro ^:private export-from [ns & syms]
  `(do ~@(for [s syms] `(def ~s ~(symbol (name ns) (name s))))))

(export-from xmas.cmd
  cur update-cur set-point edit msg msg-error
  forward-char backward-char
  beginning-of-line end-of-line
  beginning-of-buffer end-of-buffer
  forward-word backward-word
  insert-newline delete-char delete-backward-char)

(defn- line-move [dir]
  (fn [s]
    (set-point s
      (fn [t p]
        (let [ln  (gap/line-of t p)
              col (text/display-width t (gap/nth-line-start t ln) p)
              nxt (+ ln dir)]
          (cond
            (neg? nxt)                     0
            (>= nxt (gap/line-count t))    (count t)
            :else (text/pos-at-col t (gap/nth-line-start t nxt)
                                     (gap/nth-line-end t nxt) col)))))))

(def next-line     (line-move 1))
(def previous-line (line-move -1))

(defn- scroll-by [line-cmd]
  (fn [s] (nth (iterate line-cmd s) (max 1 (- (:rows s 24) 2)))))

(def scroll-down (scroll-by next-line))
(def scroll-up   (scroll-by previous-line))

(defn self-insert [s key]
  (if (or (char? key) (string? key))
    (cmd/insert-at-point s (str key))
    s))

(def ^:private kill-ring-limit 60)
(defn- kill-push [s text]
  (update s :kill #(buf/bounded-conj (or % []) kill-ring-limit text)))

(defn- kill-range
  "Delete [from,to) from the current buffer and push the deleted text onto :kill."
  [s from to]
  (if (< from to)
    (let [t (:text (cur s))]
      (-> (edit s from to "") (kill-push (gap/substr t from to))))
    s))

(defn kill-line [s]
  (let [b (cur s) p (:point b) t (:text b)
        eol (gap/line-end t p)
        end (if (= p eol) (min (inc eol) (count t)) eol)]
    (kill-range s p end)))

(defn kill-region [s]
  (if-let [mark (:mark (cur s))]
    (let [p (:point (cur s))]
      (-> (kill-range s (min mark p) (max mark p))
          (update-cur #(assoc % :mark nil))))
    s))

(defn yank [s]
  (if (seq (:kill s))
    (let [p (:point (cur s))] (edit s p p (peek (:kill s))))
    s))

(defn set-mark [s] (update-cur s #(assoc % :mark (:point %))))

(defn- replay-cmd [src-key buf-fn label]
  (fn [s]
    (if (seq (src-key (cur s)))
      (update-cur s buf-fn)
      (msg s (str "No " label)))))

(def undo (replay-cmd :undo buf/undo "undo"))
(def redo (replay-cmd :redo buf/redo "redo"))

(defn keyboard-quit [s]
  (msg (if (:mini s)
         (assoc s :buf (:prev-buf (:mini s)) :mini nil)
         (update-cur s #(assoc % :mark nil)))
       "Quit"))

(defn goto-line [s input]
  (if-let [n (try (Integer/parseInt (str/trim input)) (catch Exception _ nil))]
    (set-point s (fn [t _]
      (gap/nth-line-start t (-> n dec (max 0) (min (dec (gap/line-count t)))))))
    (msg s "Not a number")))

(defn- atomic-spit
  "Write s to path atomically (write to sibling temp, preserve POSIX perms, then rename)."
  [^String path ^String s]
  (let [target (.toPath (.getAbsoluteFile (java.io.File. path)))
        parent (.getParent target)
        tmp    (Files/createTempFile parent ".xmas-" ".tmp"
                 (into-array java.nio.file.attribute.FileAttribute []))
        nolink (into-array java.nio.file.LinkOption [])]
    (spit (.toFile tmp) s)
    (when (Files/exists target nolink)
      (try (Files/setPosixFilePermissions tmp (Files/getPosixFilePermissions target nolink))
           (catch UnsupportedOperationException _)))
    (Files/move tmp target
      (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))))

(defn save-buffer [s]
  (let [b (cur s)]
    (if-let [f (:file b)]
      (try (atomic-spit f (str (:text b)))
           (-> s (update-cur #(assoc % :modified false)) (msg (str "Wrote " f)))
           (catch Exception e (msg-error s "Save failed" e)))
      (msg s "No file"))))

(defn- detect-eol [^String s] (if (re-find #"\r\n" s) :crlf :lf))
(defn- normalize-eol [^String s]
  (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n")))

(defn- open-buf [s path b]
  (-> s (assoc-in [:bufs path] b) (assoc :buf path)))

(defn- file-info [filename]
  (let [f (.getAbsoluteFile (java.io.File. filename))]
    {:path (.getCanonicalPath f) :name (.getName f) :exists (.exists f)}))

(defn find-file [s filename]
  (let [{:keys [path name exists]} (file-info filename)]
    (cond
      ;; already open — just switch to it, don't clobber in-memory edits
      (contains? (:bufs s) path) (assoc s :buf path)

      exists
      (try (let [raw (slurp path)]
             (open-buf s path (-> (buf/make name (normalize-eol raw) path)
                                  (assoc :line-ending (detect-eol raw)))))
           (catch Exception e (msg-error s (str "Error reading " path) e)))

      :else (-> (open-buf s path (buf/make name "" path)) (msg "(New file)")))))

(defn switch-buffer [s name]
  (if (cmd/buf s name)
    (assoc s :buf name)
    (-> s (assoc-in [:bufs name] (buf/make name)) (assoc :buf name))))

;; --- Minibuffer ---

(def ^:private mini-buf-name " *mini*")

(defn mini-start
  ([s prompt on-done] (mini-start s prompt on-done nil))
  ([s prompt on-done completer]
   (-> s
       (assoc-in [:bufs mini-buf-name] (buf/make mini-buf-name))
       (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)
                     :history-idx -1 :completer completer})
       (assoc :buf mini-buf-name))))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input (str (:text (cur s)))]
      (-> s
          (assoc :buf prev-buf :mini nil)
          (update :mini-history #(if (and (seq input) (not= input (peek %)))
                                   (conj (or % []) input) (or % [])))
          (on-done input)))
    s))

(defn- mini-set
  "Replace minibuffer contents with entry, point at end."
  [s entry]
  (-> s (assoc-in [:bufs mini-buf-name :text] (gap/of entry))
        (assoc-in [:bufs mini-buf-name :point] (count entry))))

(defn- mini-history-move
  "Move minibuffer history selection. dir=+1 goes back (older), -1 forward (newer).
   idx=-1 means cleared (no selection); 0..n-1 picks from newest to oldest."
  [s dir]
  (let [hist (:mini-history s [])
        n    (count hist)
        idx  (get-in s [:mini :history-idx])
        new  (-> (+ idx dir) (max -1) (min (dec n)))]
    (cond
      (= new idx) s
      (neg? new)  (-> s (assoc-in [:mini :history-idx] -1) (mini-set ""))
      :else       (-> s (assoc-in [:mini :history-idx] new)
                        (mini-set (get hist (- (dec n) new)))))))

(defn- mini-history-prev [s] (mini-history-move s 1))
(defn- mini-history-next [s] (mini-history-move s -1))

(defn- file-completer
  "Tab-complete file paths. Returns {:completed str :candidates [str]}."
  [input]
  (let [f (java.io.File. (if (empty? input) "." input))
        [parent prefix] (if (.isDirectory f)
                          [f ""]
                          [(.getParentFile f) (.getName f)])
        parent (or parent (java.io.File. "."))
        children (try (vec (.list parent)) (catch Exception _ []))
        matches (if (empty? prefix)
                  children
                  (filterv #(.startsWith ^String % prefix) children))]
    (cond
      (empty? matches) {:completed input :candidates []}
      (= 1 (count matches))
      (let [completed (str (.getPath parent) "/" (first matches))
            full (java.io.File. completed)]
        {:completed (if (.isDirectory full) (str completed "/") completed)
         :candidates []})
      :else
      (let [common (reduce (fn [a b]
                             (apply str (map first (take-while (fn [[x y]] (= x y))
                                                               (map vector a b)))))
                           matches)]
        {:completed (str (.getPath parent) "/" common)
         :candidates matches}))))

(defn- mini-tab-complete [s]
  (if-let [completer (get-in s [:mini :completer])]
    (let [input (str (:text (cur s)))
          {:keys [completed candidates]} (completer input)]
      (-> s
          (mini-set completed)
          (cond-> (seq candidates) (msg (str/join " " candidates)))))
    s))

(declare handle-key)

;; --- Incremental search ---

(defn- isearch-do
  "Perform the search from current point in the given direction."
  [s]
  (let [{:keys [pattern direction]} (:isearch s)
        b (cur s) t (:text b) p (:point b)]
    (if (empty? pattern)
      s
      (let [found (if (= direction :forward)
                    (text/search-forward t pattern (min (inc p) (count t)))
                    (text/search-backward t pattern p))]
        (if found
          (set-point s (fn [_ _] found))
          (msg s (str "Failing I-search: " pattern)))))))

(defn isearch-start [s direction]
  (-> s (assoc :isearch {:pattern "" :direction direction :origin (:point (cur s))})))

(defn- isearch-accept [s]
  (dissoc s :isearch))

(defn- isearch-cancel [s]
  (let [origin (get-in s [:isearch :origin])]
    (-> s (dissoc :isearch)
          (set-point (fn [_ _] origin))
          (msg "Quit"))))

(defn- isearch-append [s ch]
  (-> s (update-in [:isearch :pattern] str ch)
        isearch-do))

(defn- isearch-next [s direction]
  (-> s (assoc-in [:isearch :direction] direction)
        isearch-do))

(def ^:private isearch-keys
  {[:ctrl \s]  #(isearch-next % :forward)
   [:ctrl \r]  #(isearch-next % :backward)
   :return    isearch-accept
   [:ctrl \g]  isearch-cancel})

(defn- isearch-backspace [s]
  (let [pat (get-in s [:isearch :pattern])]
    (if (seq pat)
      (-> s (assoc-in [:isearch :pattern] (subs pat 0 (dec (count pat))))
            isearch-do)
      (isearch-cancel s))))

(defn- handle-isearch
  "Handle a key during incremental search. Returns updated state."
  [s key]
  (cond
    (isearch-keys key)   ((isearch-keys key) s)
    (= key :backspace)   (isearch-backspace s)
    (and (char? key) (>= (int key) 32)) (isearch-append s (str key))
    (string? key)        (isearch-append s key)
    :else                (-> (isearch-accept s) (handle-key key))))

;; --- Eval expression (M-:) ---

(defn- eval-expression [s input]
  (try (msg s (pr-str (el/eval-1 input editor)))
       (catch Exception e (msg-error s "Eval error" e))))

;; --- Bindings ---

(defn- prompt
  "Return a command that opens a minibuffer with `p` as prompt and `on-done` as handler."
  [p on-done & [completer]]
  (fn [s] (mini-start s p on-done completer)))

(defn- confirm-quit [s]
  (if (or (:exit-pending s) (not-any? :modified (vals (:bufs s))))
    (assoc s :exit true)
    (-> s (assoc :exit-pending true)
          (msg "Modified buffers exist. C-x C-c again to quit."))))

(def bindings
  {[:ctrl \f] forward-char     [:ctrl \b] backward-char
   [:ctrl \n] next-line        [:ctrl \p] previous-line
   [:ctrl \a] beginning-of-line [:ctrl \e] end-of-line
   [:meta \f] forward-word     [:meta \b] backward-word
   [:meta \<] beginning-of-buffer [:meta \>] end-of-buffer
   :up previous-line :down next-line :right forward-char :left backward-char
   :home beginning-of-line :end end-of-line
   :page-down scroll-down :page-up scroll-up
   [:ctrl \v] scroll-down [:meta \v] scroll-up
   [:meta \g] (prompt "Goto line: " goto-line)
   [:meta \:] (prompt "Eval: " eval-expression)
   :return insert-newline
   [:ctrl \d] delete-char :backspace delete-backward-char
   [:ctrl \k] kill-line [:ctrl \w] kill-region
   [:ctrl \y] yank [:ctrl \space] set-mark
   [:ctrl \s] #(isearch-start % :forward)
   [:ctrl \r] #(isearch-start % :backward)
   [:ctrl \/] undo [:meta \/] redo [:ctrl \g] keyboard-quit
   [:ctrl \x] {[:ctrl \s] save-buffer
               [:ctrl \f] (prompt "Find file: " find-file file-completer)
               [:ctrl \c] confirm-quit
               \b         (prompt "Buffer: " switch-buffer)}})

(def ^:private mini-keys
  {:return   mini-accept
   [:meta \p] mini-history-prev
   [:meta \n] mini-history-next
   :tab      mini-tab-complete})

(defn handle-key
  "Pure state transition. Handles prefix keys via :pending state.
   Works identically for terminal and web — one key at a time."
  [s key]
  (let [prev-exit-pending (:exit-pending s)
        s (assoc s :msg nil)
        s' (cond
             ;; pending prefix — resolve second key
             (:pending s)
             (let [prefix-map (:pending s) s (dissoc s :pending)]
               (if-let [cmd (get prefix-map key)]
                 (cmd s)
                 (msg s (str "prefix " key " undefined"))))

             (:isearch s) (handle-isearch s key)

             ;; minibuffer-scoped keys
             (and (:mini s) (mini-keys key)) ((mini-keys key) s)

             :else
             (let [binding (or (get bindings key) (get (:el-bindings s) key))]
               (cond
                 (map? binding) (assoc s :pending binding)
                 (fn? binding)  (binding s)
                 (char? key)    (if (>= (int key) 32) (self-insert s key) s)
                 (string? key)  (self-insert s key)
                 :else          (msg s (str key " undefined")))))]
    ;; :exit-pending survives only the command that set it.
    (cond-> s'
      (and (not (:pending s')) prev-exit-pending (:exit-pending s'))
      (dissoc :exit-pending))))

;; --- Main ---

(defn command-loop []
  (loop []
    (let [{:keys [scroll hscroll]} (view/render @editor)]
      (swap! editor (fn [s]
        (-> s (assoc :scroll scroll)
              (update-in [:bufs (:buf s)] assoc :hscroll hscroll)))))
    (let [key (t/read-key-timeout 100)]
      (if (nil? key)
        (recur)
        (do (swap! editor #(handle-key % key))
            (when-not (:exit @editor) (recur)))))))

(defn- run-all
  "Run each no-arg fn, logging and continuing on failure."
  [fns]
  (doseq [f fns]
    (try (f) (catch Exception e (log/log "shutdown:" (.getMessage e))))))

(def ^:private scratch-text
  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n")

(defn- initial-state [rows cols]
  {:buf "*scratch*"
   :bufs {"*scratch*" (buf/make "*scratch*" scratch-text nil)}
   :kill [] :msg nil :mini nil :mini-history []
   :scroll 0 :rows rows :cols cols})

(defn- load-init-files! []
  (let [load! (fn [name loader]
                (let [p (str (System/getProperty "user.home") "/.xmas/" name)]
                  (when (.exists (java.io.File. p))
                    (try (loader p) (log/log "loaded" name)
                         (catch Exception e (swap! editor msg-error name e))))))]
    (load! "init.clj" load-file)
    (load! "init.el"  #(el/eval-string (slurp %) editor))))

(defn- silence-stdio!
  "Redirect stdout/stderr so nREPL output doesn't corrupt the terminal."
  []
  (let [null (java.io.PrintStream. (java.io.OutputStream/nullOutputStream))]
    (System/setOut null)
    (System/setErr null)))

(defn- start-services! []
  (let [srv {:nrepl (nrepl/start-server :port 7888)
             :web   (web/start! editor 1234 handle-key)
             :dev   (dev/start-watcher!)}]
    (log/log "nrepl:7888 web:1234 dev:watching")
    (spit ".nrepl-port" "7888")
    srv))

(defn- stop-services! [{:keys [nrepl web dev]}]
  (run-all [#(dev/stop-watcher! dev)
            #(web/stop! web editor)
            #(nrepl/stop-server nrepl)]))

(defn -main [& _args]
  (log/init!) (log/log "xmas starting")
  (t/install-shutdown-hook!)
  (try
    (t/enter-raw-mode!) (t/cls)
    (t/on-resize! (fn [rows cols] (swap! editor assoc :rows rows :cols cols)))
    (let [[rows cols] (t/terminal-size)]
      (reset! editor (initial-state rows cols))
      (load-init-files!)
      (silence-stdio!)
      (let [services (start-services!)]
        (try (command-loop)
             (finally (stop-services! services)))))
    (finally
      (run-all [#(log/log "xmas exiting") log/close! t/teardown!]))))
