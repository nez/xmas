(ns xmas.ed
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.view :as view]
            [xmas.log :as log]
            [nrepl.server :as nrepl]
            [xmas.web :as web]
            [xmas.dev :as dev])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute PosixFilePermissions])
  (:gen-class))

;; --- Editor state ---

(def editor (atom nil))

(defn cur [s] (get (:bufs s) (:buf s)))
(defn update-cur [s f] (update-in s [:bufs (:buf s)] f))
(defn msg [s message] (assoc s :msg message))
(defn set-point [s f] (update-cur s #(buf/set-point % f)))
(defn edit [s from to repl] (update-cur s #(buf/edit % from to repl)))

;; --- Commands (state → state) ---

(defn forward-char [s]  (set-point s (fn [t p] (text/next-pos t p))))
(defn backward-char [s] (set-point s (fn [t p] (text/prev-pos t p))))
(defn beginning-of-line [s] (set-point s text/line-start))
(defn end-of-line [s]       (set-point s text/line-end))
(defn beginning-of-buffer [s] (set-point s (fn [_ _] 0)))
(defn end-of-buffer [s]       (set-point s (fn [t _] (count t))))

(defn next-line [s]
  (set-point s (fn [t p]
    (let [col (text/display-width t (text/line-start t p) p)
          nxt (min (inc (text/line-end t p)) (count t))]
      (if (>= nxt (count t)) (count t)
        (text/pos-at-col t nxt (text/line-end t nxt) col))))))

(defn previous-line [s]
  (set-point s (fn [t p]
    (let [col (text/display-width t (text/line-start t p) p)
          ls (text/line-start t p)]
      (if (<= ls 0) 0
        (let [pe (dec ls)]
          (text/pos-at-col t (text/line-start t pe) pe col)))))))

(defn scroll-down [s]
  (let [n (max 1 (- (:rows s 24) 2))]
    (nth (iterate next-line s) n)))

(defn scroll-up [s]
  (let [n (max 1 (- (:rows s 24) 2))]
    (nth (iterate previous-line s) n)))

(defn forward-word [s]  (set-point s text/word-forward))
(defn backward-word [s] (set-point s text/word-backward))

(defn self-insert [s]
  (let [k (:last-key s) p (:point (cur s))]
    (cond (char? k)   (edit s p p (str k))
          (string? k) (edit s p p k)
          :else s)))

(defn insert-newline [s] (let [p (:point (cur s))] (edit s p p "\n")))

(defn delete-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (< p (count t)) (edit s p (text/next-pos t p) "") s)))

(defn delete-backward-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (> p 0) (edit s (text/prev-pos t p) p "") s)))

(defn- kill-push [s text]
  (update s :kill #(conj (if (>= (count %) 60) (subvec % 1) %) text)))

(defn kill-line [s]
  (let [b (cur s) p (:point b) t (:text b)
        eol (text/line-end t p)
        end (if (= p eol) (min (inc eol) (count t)) eol)]
    (if (< p end)
      (-> (edit s p end "") (kill-push (subs t p end)))
      s)))

(defn kill-region [s]
  (let [b (cur s) mark (:mark b)]
    (if-not mark s
      (let [from (min mark (:point b)) to (max mark (:point b))]
        (-> (edit s from to "")
            (kill-push (subs (:text b) from to))
            (update-cur #(assoc % :mark nil)))))))

(defn yank [s]
  (if (seq (:kill s))
    (let [p (:point (cur s))] (edit s p p (peek (:kill s))))
    s))

(defn set-mark [s] (update-cur s #(assoc % :mark (:point %))))

(defn undo [s]
  (if (seq (:undo (cur s)))
    (update-cur s buf/undo)
    (msg s "No undo")))

(defn redo [s]
  (if (seq (:redo (cur s)))
    (update-cur s buf/redo)
    (msg s "No redo")))

(defn keyboard-quit [s]
  (if (:mini s)
    (-> s (assoc :buf (:prev-buf (:mini s)) :mini nil) (msg "Quit"))
    (-> s (update-cur #(assoc % :mark nil)) (msg "Quit"))))

(defn goto-line [s input]
  (if-let [n (try (Integer/parseInt (str/trim input)) (catch Exception _ nil))]
    (set-point s (fn [t _]
      (let [target (max 1 n)]
        (loop [pos 0 line 1]
          (cond
            (>= line target) pos
            (>= pos (count t)) pos
            :else (recur (min (inc (text/line-end t pos)) (count t)) (inc line)))))))
    (msg s "Not a number")))

(defn save-buffer [s]
  (let [b (cur s)]
    (if-let [f (:file b)]
      (try
        (let [target (.toPath (java.io.File. f))
              parent (.getParent target)
              tmp (Files/createTempFile parent ".xmas-" ".tmp" (into-array java.nio.file.attribute.FileAttribute []))]
          (spit (.toFile tmp) (:text b))
          (when (Files/exists target (into-array java.nio.file.LinkOption []))
            (try (Files/setPosixFilePermissions tmp (Files/getPosixFilePermissions target (into-array java.nio.file.LinkOption [])))
                 (catch UnsupportedOperationException _)))
          (Files/move tmp target (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
          (-> s (update-cur #(assoc % :modified false)) (msg (str "Wrote " f))))
        (catch Exception e
          (msg s (str "Save failed: " (.getMessage e)))))
      (msg s "No file"))))

(defn find-file [s filename]
  (let [path (.getCanonicalPath (java.io.File. filename))
        name (or (last (str/split path #"/")) path)]
    (if (.exists (java.io.File. path))
      (try (let [raw (slurp path)
                 crlf (re-find #"\r\n" raw)
                 content (-> raw (str/replace "\r\n" "\n") (str/replace "\r" "\n"))]
             (-> s (assoc-in [:bufs path] (-> (buf/make name content path)
                                              (assoc :line-ending (if crlf :crlf :lf))))
                   (assoc :buf path)))
           (catch Exception e
             (msg s (str "Error reading " path ": " (.getMessage e)))))
      (-> s (assoc-in [:bufs path] (buf/make name "" path)) (assoc :buf path)
            (msg (str "(New file)"))))))

(defn switch-buffer [s name]
  (if (get (:bufs s) name)
    (assoc s :buf name)
    (-> s (assoc-in [:bufs name] (buf/make name)) (assoc :buf name))))

;; --- Minibuffer ---

(defn mini-start
  ([s prompt on-done] (mini-start s prompt on-done nil))
  ([s prompt on-done completer]
   (let [mb " *mini*"]
     (-> s
         (assoc-in [:bufs mb] (buf/make mb))
         (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)
                       :history-idx -1 :completer completer})
         (assoc :buf mb)))))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input (:text (cur s))]
      (-> s
          (assoc :buf prev-buf :mini nil)
          (update :mini-history #(if (and (seq input) (not= input (peek %)))
                                   (conj (or % []) input) (or % [])))
          (on-done input)))
    s))

(defn- mini-history-prev [s]
  (let [hist (:mini-history s [])
        idx (get-in s [:mini :history-idx])
        new-idx (min (inc idx) (dec (count hist)))]
    (if (and (seq hist) (< idx (dec (count hist))))
      (let [entry (get hist (- (dec (count hist)) new-idx))]
        (-> s (assoc-in [:mini :history-idx] new-idx)
              (assoc-in [:bufs " *mini*" :text] entry)
              (assoc-in [:bufs " *mini*" :point] (count entry))))
      s)))

(defn- mini-history-next [s]
  (let [idx (get-in s [:mini :history-idx])]
    (if (> idx 0)
      (let [hist (:mini-history s [])
            new-idx (dec idx)
            entry (get hist (- (dec (count hist)) new-idx))]
        (-> s (assoc-in [:mini :history-idx] new-idx)
              (assoc-in [:bufs " *mini*" :text] entry)
              (assoc-in [:bufs " *mini*" :point] (count entry))))
      (-> s (assoc-in [:mini :history-idx] -1)
            (assoc-in [:bufs " *mini*" :text] "")
            (assoc-in [:bufs " *mini*" :point] 0)))))

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
    (let [input (:text (cur s))
          {:keys [completed candidates]} (completer input)]
      (-> s
          (assoc-in [:bufs " *mini*" :text] completed)
          (assoc-in [:bufs " *mini*" :point] (count completed))
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

(defn- handle-isearch
  "Handle a key during incremental search. Returns updated state."
  [s key]
  (cond
    (= key [:ctrl \s])  (isearch-next s :forward)
    (= key [:ctrl \r])  (isearch-next s :backward)
    (= key :return)     (isearch-accept s)
    (= key [:ctrl \g])  (isearch-cancel s)
    (= key :backspace)  (let [pat (get-in s [:isearch :pattern])]
                          (if (seq pat)
                            (-> s (assoc-in [:isearch :pattern] (subs pat 0 (dec (count pat))))
                                  isearch-do)
                            (isearch-cancel s)))
    (and (char? key) (>= (int key) 32)) (isearch-append s (str key))
    (string? key)       (isearch-append s key)
    :else               (-> (isearch-accept s) (handle-key key))))

;; --- Bindings ---

(def bindings
  {[:ctrl \f] forward-char,   [:ctrl \b] backward-char
   [:ctrl \n] next-line,      [:ctrl \p] previous-line
   [:ctrl \a] beginning-of-line, [:ctrl \e] end-of-line
   [:meta \f] forward-word,   [:meta \b] backward-word
   [:meta \<] beginning-of-buffer, [:meta \>] end-of-buffer
   :up previous-line, :down next-line, :right forward-char, :left backward-char
   :home beginning-of-line, :end end-of-line
   :page-down scroll-down, :page-up scroll-up
   [:ctrl \v] scroll-down, [:meta \v] scroll-up
   [:meta \g] (fn [s] (mini-start s "Goto line: " goto-line))
   :return insert-newline
   [:ctrl \d] delete-char, :backspace delete-backward-char
   [:ctrl \k] kill-line, [:ctrl \w] kill-region
   [:ctrl \y] yank, [:ctrl \space] set-mark
   [:ctrl \s] (fn [s] (isearch-start s :forward))
   [:ctrl \r] (fn [s] (isearch-start s :backward))
   [:ctrl \/] undo, [:meta \/] redo, [:ctrl \g] keyboard-quit
   [:ctrl \x] {[:ctrl \s] save-buffer
               [:ctrl \f] (fn [s] (mini-start s "Find file: " find-file file-completer))
               [:ctrl \c] (fn [s]
                            (if (or (:exit-pending s)
                                    (not-any? :modified (vals (:bufs s))))
                              (assoc s :exit true)
                              (-> s (assoc :exit-pending true)
                                    (msg "Modified buffers exist. C-x C-c again to quit."))))
               \b         (fn [s] (mini-start s "Buffer: " switch-buffer))}})

(defn handle-key
  "Pure state transition. Handles prefix keys via :pending state.
   Works identically for terminal and web — one key at a time."
  [s key]
  (let [s (assoc s :last-key key :msg nil :exit-pending nil)]
    (cond
      ;; pending prefix — resolve second key
      (:pending s)
      (let [prefix-map (:pending s)
            s (dissoc s :pending)]
        (if-let [cmd (get prefix-map key)]
          (cmd s)
          (msg s (str "prefix " key " undefined"))))

      ;; incremental search mode
      (:isearch s)
      (handle-isearch s key)

      ;; minibuffer keys
      (and (:mini s) (= key :return))
      (mini-accept s)

      (and (:mini s) (= key [:meta \p]))
      (mini-history-prev s)

      (and (:mini s) (= key [:meta \n]))
      (mini-history-next s)

      (and (:mini s) (= key :tab))
      (mini-tab-complete s)

      ;; normal dispatch
      :else
      (let [binding (get bindings key)]
        (cond
          (map? binding) (assoc s :pending binding)  ;; store prefix, wait for next key
          (fn? binding)  (binding s)
          (char? key)    (if (>= (int key) 32) (self-insert s) s)
          (string? key)  (self-insert s)
          :else          (msg s (str key " undefined")))))))

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

(defn -main [& _args]
  (log/init!) (log/log "xmas starting")
  (t/install-shutdown-hook!)
  (try
    (t/enter-raw-mode!) (t/cls)
    (t/on-resize! (fn [rows cols]
      (swap! editor assoc :rows rows :cols cols)))
    (let [[rows cols] (t/terminal-size)]
      (reset! editor
        {:buf "*scratch*"
         :bufs {"*scratch*" (buf/make "*scratch*"
                  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n" nil)}
         :kill [] :msg nil :mini nil :mini-history []
         :scroll 0 :rows rows :cols cols :last-key nil})
      (when-let [f (let [p (str (System/getProperty "user.home") "/.xmas/init.clj")]
                     (when (.exists (java.io.File. p)) p))]
        (try (load-file f)
             (catch Exception e (swap! editor msg (str "Init: " (.getMessage e))))))
      ;; Redirect stdout/stderr so nREPL output doesn't corrupt the terminal
      (let [null-out (java.io.PrintStream. (java.io.OutputStream/nullOutputStream))]
        (System/setOut null-out)
        (System/setErr null-out))
      (let [nrepl-srv (nrepl/start-server :port 7888)
            web-srv (web/start! editor 1234 handle-key)
            dev-watcher (dev/start-watcher!)]
        (log/log "nrepl:7888 web:1234 dev:watching")
        (spit ".nrepl-port" "7888")
        (try (command-loop)
          (finally
            (run-all [#(dev/stop-watcher! dev-watcher)
                      #(web/stop! web-srv editor)
                      #(nrepl/stop-server nrepl-srv)])))))
    (finally
      (run-all [#(log/log "xmas exiting")
                log/close!
                t/reset-sg t/cls #(t/move 0 0) t/show-cur t/flush!
                t/exit-raw-mode!]))))
