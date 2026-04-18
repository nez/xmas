(ns xmas.ed
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.buflist :as buflist]
            [xmas.cmd :as cmd]
            [xmas.dired :as dired]
            [xmas.el :as el]
            [xmas.gap :as gap]
            [xmas.mode :as mode]
            [xmas.pkg :as pkg]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.view :as view]
            [xmas.window :as win]
            [xmas.log :as log]
            [nrepl.server :as nrepl]
            [xmas.web :as web]
            [xmas.dev :as dev])
  (:import [java.nio.file Files StandardCopyOption])
  (:gen-class))

;; --- Editor state ---
;; `defonce` so the dev file-watcher's `require :reload` doesn't replace
;; the live atom with a fresh nil one, wiping the running editor state.

(defonce editor (atom nil))

(defmacro ^:private export-from [ns & syms]
  `(do ~@(for [s syms] `(def ~s ~(symbol (name ns) (name s))))))

(export-from xmas.cmd
  cur update-cur set-point edit msg msg-error
  beginning-of-line end-of-line
  beginning-of-buffer end-of-buffer
  insert-newline)

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

(def ^:private next-line-raw     (line-move 1))
(def ^:private previous-line-raw (line-move -1))

(defn- scroll-by [line-cmd]
  (fn [s] (nth (iterate line-cmd s) (max 1 (- (:rows s 24) 2)))))

(def scroll-down (scroll-by next-line-raw))
(def scroll-up   (scroll-by previous-line-raw))

;; --- Prefix argument (C-u) ---

(defn- consume-arg
  "Return [n s'] where n is the active numeric prefix arg (default `d`) and
   s' has :prefix-arg cleared."
  [s d]
  (let [pa (:prefix-arg s)
        n  (or (:num pa) (:mul pa) d)]
    [n (dissoc s :prefix-arg)]))

(defn universal-argument
  "C-u: start or extend the prefix argument. Each repeat multiplies by 4."
  [s]
  (assoc s :prefix-arg {:mul (* 4 (or (:mul (:prefix-arg s)) 1))}))

(defn- repeat-cmd
  "Apply `f` to `s` `n` times (non-negative)."
  [f s n]
  (nth (iterate f s) (max 0 (long n))))

(defn- prefix-repeat
  "Wrap a (state)→state command so it honors :prefix-arg by running n times."
  [f] (fn [s] (let [[n s] (consume-arg s 1)] (repeat-cmd f s n))))

(defn forward-char
  ([s]   (let [[n s] (consume-arg s 1)] (cmd/forward-char s n)))
  ([s n] (cmd/forward-char s n)))

(defn backward-char
  ([s]   (let [[n s] (consume-arg s 1)] (cmd/backward-char s n)))
  ([s n] (cmd/backward-char s n)))

(def forward-word         (prefix-repeat cmd/forward-word))
(def backward-word        (prefix-repeat cmd/backward-word))
(def next-line            (prefix-repeat next-line-raw))
(def previous-line        (prefix-repeat previous-line-raw))
(def delete-char          (prefix-repeat cmd/delete-char))
(def delete-backward-char (prefix-repeat cmd/delete-backward-char))

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

(declare ensure-fallback-buf)

(defn keyboard-quit [s]
  (msg (if (:mini s)
         ;; If the saved prev-buf was killed while the minibuffer was
         ;; open, fall back so :buf never ends up pointing at a missing
         ;; entry.
         (let [[s target] (ensure-fallback-buf s (:prev-buf (:mini s)))]
           (assoc s :buf target :mini nil))
         (update-cur s #(assoc % :mark nil)))
       "Quit"))

(defn goto-line [s input]
  (if-let [n (try (Integer/parseInt (str/trim input)) (catch Exception _ nil))]
    (set-point s (fn [t _]
      (gap/nth-line-start t (-> n dec (max 0) (min (dec (gap/line-count t)))))))
    (msg s "Not a number")))

(defn- atomic-spit
  "Write s to path atomically (write to sibling temp, preserve POSIX perms, then rename).
   If any step after tmp creation fails, the tmp file is removed so a
   read-only dir or disk-full scenario doesn't leave `.xmas-*.tmp` debris."
  [^String path ^String s]
  (let [target (.toPath (.getAbsoluteFile (java.io.File. path)))
        parent (.getParent target)
        tmp    (Files/createTempFile parent ".xmas-" ".tmp"
                 (into-array java.nio.file.attribute.FileAttribute []))
        nolink (into-array java.nio.file.LinkOption [])
        done?  (atom false)]
    (try
      (spit (.toFile tmp) s :encoding "UTF-8")
      (when (Files/exists target nolink)
        (try (Files/setPosixFilePermissions tmp (Files/getPosixFilePermissions target nolink))
             (catch UnsupportedOperationException _)))
      (Files/move tmp target
        (into-array [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
      (reset! done? true)
      (finally
        (when-not @done?
          (try (Files/deleteIfExists tmp) (catch Exception _)))))))

(defn- auto-save-path
  "#file# backup path for a buffer file."
  [^String file]
  (let [f (java.io.File. file)
        parent (or (.getParent f) ".")]
    (str parent "/#" (.getName f) "#")))

(def ^:private auto-save-threshold 300)

;; --- Shell command (M-!) ---

(defn- run-shell
  "Run `cmd` via /bin/sh -c. If `stdin` is given, pipes it in. Returns combined
   stdout + stderr."
  [^String cmd & [^String stdin]]
  (try
    (let [pb (doto (ProcessBuilder. ["sh" "-c" cmd])
               (.redirectErrorStream true))
          p  (.start pb)]
      (if stdin
        (with-open [os (.getOutputStream p)]
          (.write os (.getBytes stdin "UTF-8")))
        (.close (.getOutputStream p)))
      (.waitFor p)
      (slurp (.getInputStream p) :encoding "UTF-8"))
    (catch Exception e (str "Error: " (.getMessage e)))))

(defn shell-command-on-region
  "Pipe the current region through `cmd` and replace it with the command's
   output. Requires an active mark."
  [s cmd]
  (let [b (cur s)
        mark (:mark b)
        point (:point b)]
    (cond
      (str/blank? cmd) s
      (nil? mark)      (msg s "No region")
      :else
      (let [from (min (long mark) (long point))
            to   (max (long mark) (long point))
            input  (gap/substr (:text b) from to)
            output (str/trimr (run-shell cmd input))]
        (-> s (edit from to output)
              (update-cur #(assoc % :mark nil)))))))

(defn shell-command
  "Execute `cmd` in a subshell. Short output goes to the echo area; long
   output opens *Shell Command Output*."
  [s cmd]
  (if (str/blank? cmd)
    s
    (let [output (str/trimr (run-shell cmd))
          multiline? (.contains output "\n")]
      (if multiline?
        (let [name "*Shell Command Output*"]
          (-> s (assoc-in [:bufs name] (buf/make name output nil))
                (cmd/set-cur-buffer name)))
        (msg s output)))))

(defn- auto-save-due
  "Pure: return [[buf-name file text] ...] for every buffer at or above the
   auto-save threshold."
  [s]
  (keep (fn [[name b]]
          (when (and (:file b) (>= (or (:edit-count b) 0) auto-save-threshold))
            [name (:file b) (str (:text b))]))
        (:bufs s)))

(defn- reset-edit-counts
  "Pure: reset :edit-count to 0 for each buf still at or above threshold.
   Guards against racing edits: a concurrent save-buffer that already
   zeroed the counter is respected."
  [s due]
  (reduce (fn [s [name _ _]]
            (cond-> s
              (>= (or (get-in s [:bufs name :edit-count]) 0) auto-save-threshold)
              (assoc-in [:bufs name :edit-count] 0)))
          s due))

(defn- spit-due!
  "Write each [_ file text] tuple to its auto-save path, swallowing errors.
   Uses atomic-spit so a crash mid-write can't leave the backup truncated —
   the whole point of the backup is to be usable after unclean shutdown."
  [due]
  (doseq [[_ file text] due]
    (try (atomic-spit (auto-save-path file) text)
         (catch Exception _))))

(defn auto-save!
  "For each file-backed buffer whose edit-count crossed the threshold,
   write its #name# backup and reset the counter. Returns the updated state.
   Do not call from inside a `swap!` — a CAS retry would re-spit the file."
  [s]
  (let [due (auto-save-due s)]
    (spit-due! due)
    (reset-edit-counts s due)))

(defn- auto-save-tick!
  "Drive auto-save against the editor atom. Spits happen once per tick
   (outside `swap!`), then the counter reset is swapped in atomically."
  [editor-atom]
  (let [due (auto-save-due @editor-atom)]
    (when (seq due)
      (spit-due! due)
      (swap! editor-atom reset-edit-counts due))))

(defn save-buffer [s]
  (let [b (cur s)
        f (:file b)]
    (if (str/blank? f)
      (msg s "No file")
      (try (atomic-spit f (str (:text b)))
           (let [bak (java.io.File. (auto-save-path f))]
             (when (.exists bak) (.delete bak)))
           (-> s (update-cur #(assoc % :modified false :edit-count 0))
                 (msg (str "Wrote " f)))
           (catch Exception e (msg-error s "Save failed" e))))))

(defn- detect-eol [^String s] (if (re-find #"\r\n" s) :crlf :lf))
(defn- normalize-eol [^String s]
  (-> s (str/replace "\r\n" "\n") (str/replace "\r" "\n")))

(defn- open-buf [s path b]
  (let [s (-> s (assoc-in [:bufs path] b) (cmd/set-cur-buffer path))
        m (mode/mode-for-file path)]
    (cond-> s m (mode/set-major-mode m))))

(defn- file-info [filename]
  (let [f (.getAbsoluteFile (java.io.File. filename))]
    {:path (.getCanonicalPath f) :name (.getName f)
     :exists (.exists f) :dir? (.isDirectory f)}))

(defn find-file [s filename]
  (let [{:keys [path name exists dir?]} (file-info filename)]
    (cond
      ;; already open — just switch to it, don't clobber in-memory edits
      (contains? (:bufs s) path) (cmd/set-cur-buffer s path)

      ;; Opening a directory drops into dired, matching Emacs's C-x C-f.
      dir? (dired/open s path)

      exists
      (try (let [raw (slurp path :encoding "UTF-8")]
             (open-buf s path (-> (buf/make name (normalize-eol raw) path)
                                  (assoc :line-ending (detect-eol raw)))))
           (catch Exception e (msg-error s (str "Error reading " path) e)))

      :else (-> (open-buf s path (buf/make name "" path)) (msg "(New file)")))))

(defn switch-buffer [s name]
  (cond
    (str/blank? name) s
    ;; Reserve internal buffer names (leading space) — letting the user
    ;; switch into " *mini*" would wedge the minibuffer machinery on the
    ;; next prompt.
    (.startsWith ^String name " ") (msg s (str "Reserved buffer name: " name))
    (cmd/buf s name) (cmd/set-cur-buffer s name)
    :else (-> s (assoc-in [:bufs name] (buf/make name)) (cmd/set-cur-buffer name))))

;; --- Minibuffer ---

(def ^:private mini-buf-name " *mini*")

(defn mini-start
  ([s prompt on-done] (mini-start s prompt on-done nil))
  ([s prompt on-done completer]
   (if (:mini s)
     ;; A minibuffer is already active. Opening a second one would drop the
     ;; outer prompt's `prev-buf` / `on-done` and leave the editor wedged.
     (msg s "Command attempted to use disabled command in minibuffer")
     (-> s
         (assoc-in [:bufs mini-buf-name] (buf/make mini-buf-name))
         (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)
                       :history-idx -1 :completer completer})
         (assoc :buf mini-buf-name)))))

(defn- ensure-fallback-buf
  "Return [state target] — pick a buffer to restore to when the saved
   target is gone. Prefers an existing non-hidden buffer, otherwise
   creates *scratch* so :buf never points at a missing entry."
  [s prev-buf]
  (cond
    (contains? (:bufs s) prev-buf) [s prev-buf]
    :else
    (if-let [n (first (remove #(.startsWith ^String % " ") (keys (:bufs s))))]
      [s n]
      [(assoc-in s [:bufs "*scratch*"] (buf/make "*scratch*")) "*scratch*"])))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input  (str (:text (cur s)))
          [s target] (ensure-fallback-buf s prev-buf)]
      (-> s
          (assoc :buf target :mini nil)
          (cond-> (and (seq input) (not= input (peek (:mini-history s))))
            (update :mini-history conj input))
          (on-done input)))
    s))

(defn- mini-set
  "Replace minibuffer contents with entry, point at end."
  [s entry]
  (update-in s [:bufs mini-buf-name] assoc
             :text (gap/of entry) :point (count entry)))

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

(defn- longest-common-prefix [xs]
  (reduce (fn [a b]
            (apply str (map first (take-while (fn [[x y]] (= x y))
                                              (map vector a b)))))
          xs))

(defn- complete-prefix
  "Core prefix-completion cascade. Returns {:completed :candidates}. `format-one`
   is applied to a unique match to decorate it (e.g. trailing slash for dirs)."
  ([input candidates] (complete-prefix input candidates identity))
  ([input candidates format-one]
   (let [matches (filterv #(.startsWith ^String % input) candidates)]
     (cond
       (empty? matches)      {:completed input :candidates []}
       (= 1 (count matches)) {:completed (format-one (first matches)) :candidates []}
       :else                 {:completed (longest-common-prefix matches)
                              :candidates matches}))))

(defn- file-completer
  "Tab-complete file paths. Returns {:completed str :candidates [str]}."
  ([input] (file-completer input nil))
  ([input _s]
   (let [f (java.io.File. (if (empty? input) "." input))
         [parent prefix] (if (.isDirectory f)
                           [f ""]
                           [(.getParentFile f) (.getName f)])
         parent (or parent (java.io.File. "."))
         children (try (vec (.list parent)) (catch Exception _ []))
         prefixed (fn [s] (str (.getPath parent) "/" s))
         {:keys [completed candidates]}
         (complete-prefix prefix children
           (fn [m] (let [full (prefixed m)]
                     (if (.isDirectory (java.io.File. full)) (str full "/") full))))]
     (cond
       (and (empty? candidates) (= completed prefix)) {:completed input :candidates []}
       (seq candidates)                               {:completed (prefixed completed) :candidates candidates}
       :else                                          {:completed completed :candidates []}))))

(defn- mini-tab-complete [s]
  (if-let [completer (get-in s [:mini :completer])]
    (let [input (str (:text (cur s)))
          {:keys [completed candidates]} (completer input s)]
      (-> s
          (mini-set completed)
          ;; Stash on :mini itself — :msg isn't rendered while a mini is
          ;; active, so the candidate list used to be invisible.
          (assoc-in [:mini :candidates] (vec candidates))))
    s))

(declare handle-key)

;; --- Incremental search ---

(defn- isearch-do
  "Search from current point in :isearch's direction. When :extend? is true
   (pattern just grew or shrank), include the current point as a candidate so
   a match anchored there can grow/shrink; otherwise advance past the current
   match so C-s/C-r finds the next occurrence."
  [s]
  (let [{:keys [pattern direction extend?]} (:isearch s)
        b (cur s) t (:text b) p (:point b)
        tn (count t) pn (count pattern)]
    (if (empty? pattern)
      s
      (let [found (if (= direction :forward)
                    (text/search-forward  t pattern (if extend? p (min (+ p pn) tn)))
                    ;; Extend: allow the match starting at p (ending at p+pn)
                    ;; to stay anchored. (inc p) only worked for pn=1 — multi-
                    ;; char patterns were yanked away from the current match.
                    (text/search-backward t pattern (if extend? (+ p pn) p)))]
        (if found
          (set-point s (fn [_ _] found))
          (msg s (str "Failing I-search: " pattern)))))))

(defn isearch-start [s direction]
  (if (:mini s)
    ;; Isearch and the minibuffer can't sensibly share an editor state —
    ;; isearch would operate on the minibuffer text, and the minibuffer's
    ;; prompt would remain live underneath. Refuse.
    (msg s "Command attempted to use disabled command in minibuffer")
    (-> s (assoc :isearch {:pattern "" :direction direction
                           :origin (:point (cur s))}))))

(defn- isearch-accept [s]
  ;; Clear :msg too — otherwise a "Failing I-search: foo" from the final
  ;; keystroke survives into the normal echo area after RET.
  (-> s (dissoc :isearch) (assoc :msg nil)))

(defn- isearch-cancel [s]
  (let [origin (get-in s [:isearch :origin])]
    (-> s (dissoc :isearch)
          (set-point (fn [_ _] origin))
          (msg "Quit"))))

(defn- isearch-append [s ch]
  (-> s (update :isearch assoc :extend? true)
        (update-in [:isearch :pattern] str ch)
        isearch-do))

(defn- isearch-next [s direction]
  (-> s (update :isearch assoc :direction direction :extend? false)
        isearch-do))

(def ^:private isearch-keys
  {[:ctrl \s]  #(isearch-next % :forward)
   [:ctrl \r]  #(isearch-next % :backward)
   :return    isearch-accept
   [:ctrl \g]  isearch-cancel})

(defn- isearch-backspace [s]
  (let [pat (get-in s [:isearch :pattern])]
    (if (seq pat)
      (-> s (update :isearch assoc :extend? true)
            (assoc-in [:isearch :pattern] (subs pat 0 (dec (count pat))))
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
  ;; Must not pass the global `editor` atom here: eval-expression runs inside
  ;; `(swap! editor ...)`, and elisp builtins that `swap! *state*` on the
  ;; global atom would be clobbered when the outer swap commits `s`. Thread
  ;; a local atom instead and merge its final value back.
  (try
    (let [a (atom s)
          result (el/eval-1 input a)]
      (msg @a (pr-str result)))
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

;; --- Window commands ---

(defn- do-split [s dir]
  (let [[tree path] (win/split (:windows s) (:cur-window s) dir)]
    (-> s (assoc :windows tree :cur-window path))))

(defn split-window-below [s] (do-split s :stacked))
(defn split-window-right [s] (do-split s :side-by-side))

(defn other-window [s]
  (let [path (win/next-leaf (:windows s) (:cur-window s))]
    (cmd/switch-window s path)))

(defn delete-window [s]
  (let [s (cmd/save-cur-point s)
        [tree path] (win/delete-window (:windows s) (:cur-window s))]
    (-> s (assoc :windows tree) (cmd/switch-window path))))

(defn delete-other-windows [s]
  (let [s (cmd/save-cur-point s)
        [tree path] (win/only (:windows s) (:cur-window s))]
    (-> s (assoc :windows tree :cur-window path))))

(defn- resize [s dir delta]
  (let [total (case dir :stacked (:rows s) :side-by-side (:cols s))
        tree' (win/adjust-size (:windows s) (:cur-window s) dir total delta)]
    (assoc s :windows tree')))

(defn enlarge-window-horizontally [s] (resize s :side-by-side  1))
(defn shrink-window-horizontally  [s] (resize s :side-by-side -1))
(defn enlarge-window              [s] (resize s :stacked       1))
(defn shrink-window               [s] (resize s :stacked      -1))

;; --- Query replace (M-%) ---

(declare handle-key)

(defn- regex-find
  "Return [start end groups] for the next match of `pattern` at or after `from`, or nil."
  [^CharSequence t ^String pattern ^long from]
  (try
    (let [m (re-matcher (re-pattern pattern) t)]
      (when (.find m (int from))
        [(.start m) (.end m)
         (vec (for [i (range (inc (.groupCount m)))]
                (or (.group m (int i)) "")))]))
    (catch Exception _ nil)))

(defn- expand-replacement
  "Replace \\N in `template` with the corresponding match group."
  [^String template groups]
  (str/replace template #"\\(\d)"
               (fn [[_ d]] (or (get groups (Integer/parseInt d)) ""))))

(defn- qr-msg [s]
  (let [{:keys [from to regex?]} (:query-replace s)]
    (msg s (str (if regex? "Query replacing regexp " "Query replacing ")
                from " with " to ": (y/n/!/q)"))))

(defn- qr-exit [s]
  (let [n (get-in s [:query-replace :count] 0)]
    (-> s (dissoc :query-replace) (msg (str "Replaced " n " occurrence(s)")))))

(defn- qr-find-next [s]
  (let [{:keys [from regex?]} (:query-replace s)
        t (:text (cur s))
        p (:point (cur s))]
    (if regex?
      (regex-find t from p)
      (when-let [start (text/search-forward t from p)]
        [start (+ start (count from)) nil]))))

(defn- qr-advance [s]
  (if-let [[start end groups] (qr-find-next s)]
    (-> s (cmd/goto-char start)
          (update :query-replace assoc :match-end end :groups groups)
          qr-msg)
    (qr-exit s)))

(defn- qr-replace-here [s]
  (let [{:keys [to regex? match-end groups]} (:query-replace s)
        p (:point (cur s))
        replacement (if regex? (expand-replacement to groups) to)
        zero-width? (= p match-end)
        s2 (-> s (edit p match-end replacement)
                 (update-in [:query-replace :count] (fnil inc 0)))]
    (if zero-width?
      ;; Zero-width match (e.g. regex `$` or `a*`): the next find would
      ;; refind the same position. Advance point past it; if at EOF, exit.
      (let [b (cur s2) pt (:point b) tn (count (:text b))]
        (if (>= pt tn)
          (qr-exit s2)
          (qr-advance (update-cur s2 #(update % :point inc)))))
      (qr-advance s2))))

(defn- qr-replace-all [s]
  (loop [s s]
    (if (:query-replace s)
      (recur (if (qr-find-next s) (qr-replace-here s) (qr-exit s)))
      s)))

(defn- query-replace-begin
  "Enter query-replace mode at the first match from point."
  [s from to regex?]
  (let [s (assoc s :query-replace {:from from :to to :count 0 :regex? regex?})]
    (if (qr-find-next s)
      (qr-advance s)
      (msg (dissoc s :query-replace) "No match"))))

(defn query-replace
  "M-% entry point: prompt for FROM, then TO, then enter the interactive loop."
  [s input]
  (if (str/blank? input)
    s
    (mini-start s (str "Query replace " input " with: ")
      (fn [s to] (query-replace-begin s input to false)))))

(defn query-replace-regexp
  "M-x entry point for regex-based query replace."
  [s input]
  (if (str/blank? input)
    s
    (mini-start s (str "Query replace regexp " input " with: ")
      (fn [s to] (query-replace-begin s input to true)))))

(defn query-replace-cmd [s]
  (mini-start s "Query replace: " query-replace))

(defn query-replace-regexp-cmd [s]
  (mini-start s "Query replace regexp: " query-replace-regexp))

(def ^:private qr-keys
  {\y qr-replace-here
   \n qr-advance
   \! qr-replace-all
   \q qr-exit
   :return qr-exit
   :escape qr-exit})

;; --- Keyboard macros ---

(defn start-kbd-macro [s]
  (-> s (assoc :macro-recording []) (msg "Defining kbd macro...")))

(defn end-kbd-macro [s]
  (if-let [rec (:macro-recording s)]
    ;; drop the trailing [[:ctrl \x] \)] that triggered this command
    (let [keys (vec (drop-last 2 rec))]
      (-> s (dissoc :macro-recording)
            (assoc :last-macro keys)
            (msg "Keyboard macro defined")))
    (msg s "Not defining kbd macro")))

(defn- replay-macro
  "Reduce `keys` through handle-key without recording them as themselves —
   otherwise a macro invoked while recording would inline its own keys
   into the outer recording."
  [s keys]
  (let [rec (:macro-recording s)
        s'  (reduce handle-key (dissoc s :macro-recording) keys)]
    (cond-> s' rec (assoc :macro-recording rec))))

(defn call-last-kbd-macro [s]
  (if-let [keys (:last-macro s)]
    (replay-macro s keys)
    (msg s "No kbd macro defined")))

(defn name-last-kbd-macro
  "Store the last recorded macro under `name` for later replay."
  [s name]
  (if-let [m (:last-macro s)]
    (if (str/blank? name)
      (msg s "Empty macro name")
      (-> s (assoc-in [:named-macros (str/trim name)] m)
            (msg (str "Named macro: " (str/trim name)))))
    (msg s "No macro to name")))

(defn- trim-name [^String s] (str/trim (or s "")))

(defn execute-kbd-macro
  "Replay the macro stored under `name`."
  [s name]
  (if-let [keys (get-in s [:named-macros (trim-name name)])]
    (replay-macro s keys)
    (msg s (str "No macro: " name))))

(defn name-last-kbd-macro-cmd
  "M-x entry point: prompt for a macro name and store it."
  [s]
  (mini-start s "Name macro: " name-last-kbd-macro))

(defn execute-kbd-macro-cmd
  "M-x entry point: prompt for a stored macro name and replay it."
  [s]
  (let [candidates (sort (keys (:named-macros s)))
        completer (fn [input _s] (complete-prefix input candidates))]
    (mini-start s "Macro name: " execute-kbd-macro completer)))

;; --- Help (C-h) ---

(declare bindings)

(defn- resolve-binding [s key]
  (or (mode/lookup-key s key)
      (get (:el-bindings s) key)
      (get bindings key)))

(defn describe-key
  "C-h k: capture the next keypress and report what it is bound to."
  [s]
  (-> s (assoc :capture-next :describe-key)
        (msg "Describe key: ")))

(defn- name-of-fn
  "Reverse-lookup a command name given its fn value."
  [s f]
  (some (fn [[n {:keys [fn]}]] (when (= fn f) n)) (:commands s)))

(defn- capture-describe-key [s key]
  (let [b (resolve-binding s key)
        desc (cond
               (map? b) "prefix key"
               (fn? b)  (if-let [n (name-of-fn s b)]
                          (str n " -- " (get-in s [:commands n :doc]))
                          "(anonymous)")
               (nil? b) "undefined"
               :else    (pr-str b))]
    (-> s (dissoc :capture-next)
          (msg (str (pr-str key) " runs " desc)))))

(defn- describe-function [s name]
  (let [n  (trim-name name)
        cmd (get-in s [:commands n])
        sym (symbol n)
        el-fns (:el-fns s)]
    (cond
      (str/blank? n) s
      cmd (msg s (str n " -- " (:doc cmd)))
      (and el-fns (contains? @el-fns sym)) (msg s (str n " -- (elisp function)"))
      :else (msg s (str "No function: " n)))))

(defn- describe-variable [s name]
  (let [n (trim-name name)
        sym (symbol n)
        vars (:el-vars s)
        entry (when vars (find @vars sym))
        doc (get-in s [:custom-docs sym])]
    (cond
      (str/blank? n) s
      entry (msg s (str n " = " (pr-str (val entry))
                        (when doc (str "  --  " doc))))
      :else (msg s (str "No variable: " n)))))


(defn- dired-find-file-at-point
  "RET in a dired buffer: open the entry under point (file → find-file,
   dir → nested dired)."
  [s]
  (if-let [[kind path] (dired/find-file-at-point s)]
    (case kind
      :dir  (dired/open s path)
      :file (find-file s path))
    s))

(def dired-keymap
  {:return dired-find-file-at-point
   \d      dired/mark-delete
   \u      dired/unmark
   \x      dired/do-delete
   \g      dired/revert})

(def buflist-keymap
  {:return buflist/switch
   \k      buflist/kill
   \g      buflist/revert})

;; --- Named-command registry (for M-x + help) ---

(declare package-list)

(defn- cmd-entry [f doc] {:fn f :doc doc})

(defn- builtin-commands []
  {"forward-char"         (cmd-entry forward-char "Move point forward one character.")
   "backward-char"        (cmd-entry backward-char "Move point backward one character.")
   "next-line"            (cmd-entry next-line "Move to the next line.")
   "previous-line"        (cmd-entry previous-line "Move to the previous line.")
   "beginning-of-line"    (cmd-entry beginning-of-line "Move to start of current line.")
   "end-of-line"          (cmd-entry end-of-line "Move to end of current line.")
   "forward-word"         (cmd-entry forward-word "Move forward one word.")
   "backward-word"        (cmd-entry backward-word "Move backward one word.")
   "beginning-of-buffer"  (cmd-entry beginning-of-buffer "Move to start of buffer.")
   "end-of-buffer"        (cmd-entry end-of-buffer "Move to end of buffer.")
   "scroll-down"          (cmd-entry scroll-down "Scroll forward one page.")
   "scroll-up"            (cmd-entry scroll-up "Scroll backward one page.")
   "insert-newline"       (cmd-entry insert-newline "Insert a newline at point.")
   "delete-char"          (cmd-entry delete-char "Delete the character after point.")
   "delete-backward-char" (cmd-entry delete-backward-char "Delete the character before point.")
   "kill-line"            (cmd-entry kill-line "Kill from point to end of line.")
   "kill-region"          (cmd-entry kill-region "Kill the region between mark and point.")
   "yank"                 (cmd-entry yank "Reinsert the most recent kill.")
   "set-mark"             (cmd-entry set-mark "Set the mark at point.")
   "undo"                 (cmd-entry undo "Undo the last change.")
   "redo"                 (cmd-entry redo "Redo the last undone change.")
   "keyboard-quit"        (cmd-entry keyboard-quit "Cancel the current input or minibuffer.")
   "save-buffer"          (cmd-entry save-buffer "Save the current buffer to its file.")
   "split-window-below"   (cmd-entry split-window-below "Split the current window into top and bottom.")
   "split-window-right"   (cmd-entry split-window-right "Split the current window into left and right.")
   "other-window"         (cmd-entry other-window "Switch to the next window.")
   "delete-window"        (cmd-entry delete-window "Delete the current window.")
   "delete-other-windows" (cmd-entry delete-other-windows "Delete all windows except the current one.")
   "enlarge-window"              (cmd-entry enlarge-window "Make the current window one row taller.")
   "shrink-window"               (cmd-entry shrink-window "Make the current window one row shorter.")
   "enlarge-window-horizontally" (cmd-entry enlarge-window-horizontally "Make the current window one column wider.")
   "shrink-window-horizontally"  (cmd-entry shrink-window-horizontally "Make the current window one column narrower.")
   "package-list"                (cmd-entry package-list "Show installed packages in the echo area.")
   "name-last-kbd-macro"         (cmd-entry name-last-kbd-macro-cmd "Save the last recorded macro under a name.")
   "execute-kbd-macro"           (cmd-entry execute-kbd-macro-cmd "Replay a named keyboard macro.")
   "query-replace"               (cmd-entry query-replace-cmd "Interactively replace literal matches.")
   "query-replace-regexp"        (cmd-entry query-replace-regexp-cmd "Interactively replace regex matches (use \\1 etc. in replacement).")})

(defn- register-builtin-commands [s]
  (assoc s :commands (builtin-commands)))

;; --- M-x ---

(defn- command-names [s]
  (sort (distinct (concat (keys (:commands s))
                          (map str (el/list-fn-names s))))))

(defn- command-completer
  "Tab-complete command names. Returns {:completed str :candidates [str]}."
  [input s]
  (complete-prefix input (command-names s)))

(defn execute-extended-command
  "Look up and invoke a named command. M-x entry point."
  [s name]
  (if (str/blank? name)
    s
    (let [n (str/trim name)
          cmd (get-in s [:commands n])
          sym (symbol n)
          el-fns (:el-fns s)]
      (cond
        cmd ((:fn cmd) s)
        (and el-fns (contains? @el-fns sym))
        (let [a (atom s)] (el/call-fn sym a) @a)
        :else (msg s (str "No command: " n))))))

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
   [:meta \x] (prompt "M-x " execute-extended-command command-completer)
   [:meta \%] (prompt "Query replace: " query-replace)
   [:meta \!] (prompt "Shell command: " shell-command)
   [:meta \|] (prompt "Shell command on region: " shell-command-on-region)
   :return insert-newline
   [:ctrl \d] delete-char :backspace delete-backward-char
   [:ctrl \k] kill-line [:ctrl \w] kill-region
   [:ctrl \y] yank [:ctrl \space] set-mark
   [:ctrl \s] #(isearch-start % :forward)
   [:ctrl \r] #(isearch-start % :backward)
   [:ctrl \/] undo [:meta \/] redo [:ctrl \g] keyboard-quit
   [:ctrl \u] universal-argument
   [:ctrl \h] {\k describe-key
               \f (prompt "Describe function: " describe-function command-completer)
               \v (prompt "Describe variable: " describe-variable)}
   [:ctrl \x] {[:ctrl \s] save-buffer
               [:ctrl \f] (prompt "Find file: " find-file file-completer)
               [:ctrl \b] buflist/open
               [:ctrl \c] confirm-quit
               \b         (prompt "Buffer: " switch-buffer)
               \d         (prompt "Dired: " dired/open file-completer)
               \0         delete-window
               \1         delete-other-windows
               \2         split-window-below
               \3         split-window-right
               \o         other-window
               \{         shrink-window-horizontally
               \}         enlarge-window-horizontally
               \^         enlarge-window
               \(         start-kbd-macro
               \)         end-kbd-macro
               \e         call-last-kbd-macro}})

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
        s (cond-> s (:macro-recording s) (update :macro-recording conj key))
        s' (cond
             ;; capture-next: consume the key as data, don't dispatch
             (= :describe-key (:capture-next s))
             (capture-describe-key (dissoc s :capture-next) key)

             ;; pending prefix — resolve second key
             (:pending s)
             (let [prefix-map (:pending s) s (dissoc s :pending)]
               (if-let [cmd (get prefix-map key)]
                 ;; The command consumes :prefix-arg just like a direct
                 ;; dispatch would; strip it here so `C-u 4 C-x C-s` works.
                 (dissoc (cmd s) :prefix-arg)
                 (msg s (str "prefix " key " undefined"))))

             (:query-replace s)
             (let [h (get qr-keys key)] ((or h qr-exit) s))

             (:isearch s) (handle-isearch s key)

             ;; minibuffer-scoped keys
             (and (:mini s) (mini-keys key)) ((mini-keys key) s)

             :else
             (let [pa (:prefix-arg s)
                   digit? (and pa (char? key) (<= (int \0) (int key) (int \9)))]
               (cond
                 digit?
                 (assoc s :prefix-arg
                        {:num (+ (* 10 (or (:num pa) 0))
                                 (- (int key) (int \0)))})
                 :else
                 (let [binding (resolve-binding s key)
                       s' (cond
                            (map? binding) (assoc s :pending binding)
                            (fn? binding)  (binding s)
                            (char? key)    (if (>= (int key) 32) (self-insert s key) s)
                            (string? key)  (self-insert s key)
                            :else          (msg s (str key " undefined")))]
                   ;; Only drop :prefix-arg when a command actually ran.
                   ;; When we stashed a prefix keymap into :pending, the
                   ;; prefix-arg still belongs to the upcoming command.
                   (cond-> s'
                     (and pa
                          (not (map? binding))
                          (not= binding universal-argument))
                     (dissoc :prefix-arg))))))]
    ;; :exit-pending survives only the command that set it.
    (cond-> s'
      (and (not (:pending s')) prev-exit-pending (:exit-pending s'))
      (dissoc :exit-pending))))

;; --- Main ---

(defn command-loop []
  (loop []
    ;; CAS the scroll writeback against the rendered snapshot so a concurrent
    ;; mutation (nREPL/web) doesn't clobber the other thread's update.
    (let [snapshot @editor
          {:keys [sig cur-scroll cur-hscroll line-state-updates]}
          (view/render snapshot)
          wp (cmd/cur-window-path snapshot)
          with-cache
          (reduce-kv
            (fn [acc buf-name {:keys [version states]}]
              (if (= version (get-in acc [:bufs buf-name :version]))
                (assoc-in acc [:bufs buf-name :line-states] states)
                acc))
            (assoc snapshot :render-sig sig)
            (or line-state-updates {}))
          updated (cond-> with-cache
                    (some? cur-scroll)  (assoc-in (conj wp :scroll) cur-scroll)
                    (some? cur-hscroll) (assoc-in (conj wp :hscroll) cur-hscroll))]
      (compare-and-set! editor snapshot updated))
    (let [key (t/read-key-timeout 100)]
      (if (nil? key)
        (recur)
        (do (swap! editor #(handle-key % key))
            (auto-save-tick! editor)
            (when-not (:exit @editor) (recur)))))))

(defn- run-all
  "Run each no-arg fn, logging and continuing on failure."
  [fns]
  (doseq [f fns]
    (try (f) (catch Exception e (log/log "shutdown:" (.getMessage e))))))

(def ^:private scratch-text
  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n")

(defn- register-builtin-modes [s]
  (-> s
      (mode/register :fundamental :major)
      (mode/register :clojure-mode :major)
      (mode/register :emacs-lisp-mode :major)
      (mode/register :dired-mode :major :keymap dired-keymap)
      (mode/register :buflist-mode :major :keymap buflist-keymap)))

(defn- initial-state [rows cols]
  (-> {:buf "*scratch*"
       :bufs {"*scratch*" (buf/make "*scratch*" scratch-text nil)}
       :kill [] :msg nil :mini nil :mini-history []
       :windows (win/leaf "*scratch*") :cur-window []
       :rows rows :cols cols}
      register-builtin-modes
      register-builtin-commands))

(defn- load-init-files! []
  (let [load! (fn [name loader]
                (let [p (str (System/getProperty "user.home") "/.xmas/" name)]
                  (when (.exists (java.io.File. p))
                    (try (loader p) (log/log "loaded" name)
                         (catch Exception e (swap! editor msg-error name e))))))]
    (load! "init.clj" load-file)
    (load! "init.el"  #(el/eval-string (slurp % :encoding "UTF-8") editor))
    (pkg/load-all! editor)))

(defn package-list
  "M-x command: show installed packages in the echo area."
  [s]
  (let [names (pkg/installed)]
    (msg s (if (seq names)
             (str "Installed: " (str/join " " names))
             "No packages installed."))))

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
