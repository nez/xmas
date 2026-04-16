(ns xmas.el
  (:refer-clojure :exclude [eval read])
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.text :as text]
            [xmas.term :as t])
  (:import [java.io PushbackReader StringReader BufferedReader InputStreamReader]
           [java.util.regex Pattern]))

;; ============================================================
;; READER
;; ============================================================

(defn- read-ch [^PushbackReader rdr]
  (let [c (.read rdr)] (when (>= c 0) (char c))))

(defn- unread-ch [^PushbackReader rdr ch]
  (.unread rdr (int ch)))

(defn- peek-ch [^PushbackReader rdr]
  (when-let [ch (read-ch rdr)] (unread-ch rdr ch) ch))

(defn- err [msg]
  (throw (ex-info msg {:condition 'error :data (list msg)})))

(def ^:private ws-chars #{\space \tab \newline \return})
(defn- ws? [ch] (contains? ws-chars ch))

(defn- skip-ws [^PushbackReader rdr]
  (loop []
    (let [ch (read-ch rdr)]
      (cond
        (nil? ch)  nil
        (ws? ch)   (recur)
        (= ch \;)  (do (loop [] (let [c (read-ch rdr)]
                                  (when (and c (not= c \newline)) (recur))))
                       (recur))
        :else      (unread-ch rdr ch)))))

(def ^:private delimiters #{\( \) \[ \] \" \' \` \, \; })

(defn- delimiter? [ch]
  (or (nil? ch) (ws? ch) (contains? delimiters ch)))

(defn- read-string-literal [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (nil? ch) (err "Unterminated string literal")
          (= ch \") (str sb)
          (= ch \\) (let [esc (read-ch rdr)]
                      (case esc
                        \\ (.append sb \\)
                        \" (.append sb \")
                        \n (.append sb \newline)
                        \t (.append sb \tab)
                        (if (nil? esc)
                          (err "Unterminated string escape")
                          (err (str "Unknown string escape: \\" esc))))
                      (recur))
          :else     (do (.append sb ch) (recur)))))))

(declare read)

;; Dotted pair: (a . b) where cdr is not a list
(deftype DottedPair [head tail]
  clojure.lang.ISeq
  (first [_] head)
  (next [_] (when (or (seq? tail) (instance? DottedPair tail)) tail))
  (more [this] (or (.next this) ()))
  (cons [this o] (clojure.lang.Cons. o this))
  clojure.lang.IPersistentCollection
  (count [_] (if (or (seq? tail) (instance? DottedPair tail)) (inc (count tail)) 2))
  (empty [_] ())
  (equiv [this o] (.equals this o))
  clojure.lang.Seqable
  (seq [this] this)
  clojure.lang.Sequential
  Object
  (equals [_ o] (and (instance? DottedPair o)
                      (= head (.head ^DottedPair o))
                      (= tail (.tail ^DottedPair o))))
  (hashCode [_] (unchecked-add-int (hash head) (unchecked-multiply-int 31 (hash tail))))
  (toString [_] (str "(" head " . " tail ")")))

(defn- read-delimited [^PushbackReader rdr close-ch label result-fn]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch)       (err (str "Unterminated " label))
        (= ch close-ch) (do (read-ch rdr) (result-fn acc))
        :else           (recur (conj acc (read rdr)))))))

(defn- read-wrapping [^PushbackReader rdr sym label]
  (let [form (read rdr)]
    (when (= form ::eof) (err (str "Unexpected EOF after " label)))
    (list sym form)))

(defn- read-unquote [^PushbackReader rdr]
  (let [ch (peek-ch rdr)]
    (if (= ch \@)
      (do (read-ch rdr)
          (list 'splice-unquote (read rdr)))
      (list 'unquote (read rdr)))))

(defn- read-char-literal [^PushbackReader rdr]
  (let [ch (read-ch rdr)]
    (cond
      (nil? ch) (err "Unexpected EOF after ?")
      (= ch \\) (let [esc (read-ch rdr)]
                  (case esc
                    \n \newline  \t \tab  \s \space  \\ \\
                    (if (nil? esc)
                      (err "Unexpected EOF in character literal")
                      (err (str "Unknown character escape: \\" esc)))))
      :else     ch)))

(defn- parse-token [^String tok]
  (if (= tok "nil") nil
    (or (try (Long/parseLong tok) (catch NumberFormatException _ nil))
        (try (Double/parseDouble tok) (catch NumberFormatException _ nil))
        (symbol tok))))

(defn- read-token [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (delimiter? ch) (do (when ch (unread-ch rdr ch))
                              (let [tok (str sb)]
                                (when (empty? tok) (err "Unexpected delimiter"))
                                (parse-token tok)))
          :else           (do (.append sb ch) (recur)))))))

(defn- read-list
  "Read a list, handling dotted pair notation (a . b)."
  [^PushbackReader rdr]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch)  (err "Unterminated list")
        (= ch \))  (do (read-ch rdr) (apply list acc))
        (= ch \.)  (do (read-ch rdr)
                     (if (delimiter? (peek-ch rdr))
                       ;; dot separator: read tail, expect close paren
                       (let [tail (read rdr)]
                         (skip-ws rdr)
                         (when (not= (read-ch rdr) \)) (err "Expected ) after dotted pair"))
                         (reduce (fn [cdr car]
                                   (if (or (nil? cdr) (seq? cdr))
                                     (clojure.core/cons car cdr)
                                     (DottedPair. car cdr)))
                                 tail (reverse acc)))
                       ;; not a dot separator — token starting with .
                       (do (unread-ch rdr \.)
                           (recur (conj acc (read rdr))))))
        :else      (recur (conj acc (read rdr)))))))

(defn- read-dispatch
  "Handle # dispatch: #'sym, #(...), #xHEX, #oOCT, #bBIN, #s(...)."
  [^PushbackReader rdr]
  (let [ch (read-ch rdr)]
    (case ch
      \' (list 'function (read rdr))
      \( (first (read-delimited rdr \) "propertized-string" identity))
      \s (read rdr)
      \x (Long/parseLong (str (read-token rdr)) 16)
      \o (Long/parseLong (str (read-token rdr)) 8)
      \b (Long/parseLong (str (read-token rdr)) 2)
      (if (nil? ch) (err "Unexpected EOF after #")
        (do (unread-ch rdr ch) (symbol (str "#" (read-token rdr))))))))

(defn read
  "Read one elisp form from a PushbackReader. Returns ::eof on end of input."
  [^PushbackReader rdr]
  (skip-ws rdr)
  (let [ch (read-ch rdr)]
    (if (nil? ch) ::eof
      (case ch
        \" (read-string-literal rdr)
        \( (read-list rdr)
        \) (err "Unexpected ')'")
        \[ (read-delimited rdr \] "vector" identity)
        \] (err "Unexpected ']'")
        \' (read-wrapping rdr 'quote "quote")
        \` (read-wrapping rdr 'backquote "backquote")
        \, (read-unquote rdr)
        \? (read-char-literal rdr)
        \# (read-dispatch rdr)
        (do (unread-ch rdr ch) (read-token rdr))))))

(defn read-all
  "Read all elisp forms from a string. Returns a vector."
  [^String s]
  (let [rdr (PushbackReader. (StringReader. s))]
    (into [] (take-while #(not= % ::eof)) (repeatedly #(read rdr)))))

;; ============================================================
;; EVALUATOR
;; ============================================================

;; Dynamic scope Lisp-2: separate var and fn namespaces.
;; *state* holds the editor state during evaluation.

(def ^:dynamic *vars* nil)      ;; atom {symbol -> value}
(def ^:dynamic *fns* nil)       ;; atom {symbol -> {:args [...] :parsed {...} :body [...]}}
(def ^:dynamic *macros* nil)    ;; atom {symbol -> {:args [...] :parsed {...} :body [...]}}
(def ^:dynamic *docs* nil)      ;; atom {symbol -> docstring}
(def ^:dynamic *features* nil)  ;; atom #{symbol}
(def ^:dynamic *load-path* nil) ;; atom [dir-string]
(def ^:dynamic *plists* nil)    ;; atom {symbol -> {prop -> val}}
(def ^:dynamic *loading* nil)   ;; atom #{symbol} — circular require guard
(def ^:dynamic *state* nil)     ;; atom of editor state
(def ^:dynamic *env* {})        ;; lexical environment: {symbol -> atom}
(def ^:dynamic *buffer-local-syms* nil) ;; atom #{symbol} — auto-buffer-local markers
(def ^:dynamic *interactive* false)

(declare eval apply-user-fn expand-backquote capture-env call-fn eval-with-env)

;; --- Buffer helpers (needed by special forms) ---

(defn- el-cur [] (get (:bufs @*state*) (:buf @*state*)))

(defn- update-buf!
  "Apply f to the current buffer in *state*."
  [f]
  (swap! *state* (fn [s] (update-in s [:bufs (:buf s)] f))))

;; --- Variable lookup (respects buffer-locals) ---

(defn- buf-locals [] (:locals (el-cur) {}))

(defn- var-lookup
  "Look up symbol in buffer-locals then globals. Returns ::not-found if unbound."
  [sym]
  (let [locals (buf-locals)]
    (if-let [e (find locals sym)]
      (val e)
      (if-let [e (find @*vars* sym)]
        (val e)
        ::not-found))))

(defn- el-var
  "Read an Elisp global variable."
  [sym] (get @*vars* sym))

(defn- el-var!
  "Set an Elisp global variable, return the value."
  [sym val] (swap! *vars* assoc sym val) val)

;; --- Argument binding ---

(defn- parse-arglist
  "Parse an elisp arglist into {:required [...] :optional [...] :rest sym-or-nil}."
  [arglist]
  (loop [args (seq arglist) mode :required required [] optional [] rest-sym nil]
    (if-not args
      {:required required :optional optional :rest rest-sym}
      (let [a (first args)]
        (cond
          (= a '&optional) (recur (next args) :optional required optional nil)
          (= a '&rest)     (if (next args)
                             {:required required :optional optional :rest (second args)}
                             (err "Missing variable after &rest"))
          (= mode :required) (recur (next args) :required (conj required a) optional nil)
          :else              (recur (next args) :optional required (conj optional a) nil))))))

(defn- bind-args
  "Bind call-args to a parsed arglist. Returns map of {sym -> val}."
  [parsed call-args fn-name]
  (let [{:keys [required optional rest]} parsed
        n-req (count required)
        n-opt (count optional)
        n-args (count call-args)]
    (when (< n-args n-req)
      (err (str "Wrong number of arguments for " fn-name
                ": expected at least " n-req ", got " n-args)))
    (when (and (nil? rest) (> n-args (+ n-req n-opt)))
      (err (str "Wrong number of arguments for " fn-name
                ": expected at most " (+ n-req n-opt) ", got " n-args)))
    (merge (zipmap required call-args)
           (zipmap optional (concat (drop n-req call-args) (repeat nil)))
           (when rest {rest (seq (drop (+ n-req n-opt) call-args))}))))

;; --- Special forms ---

(defn- make-cells
  "Wrap each value in a map into an atom for lexical env slots."
  [m]
  (into {} (map (fn [[k v]] [k (atom v)])) m))

(defn- eval-body
  "Eval a sequence of forms (progn semantics). Returns last value."
  [forms]
  (reduce (fn [_ f] (eval f)) nil forms))

(defn- el-signal
  "Signal an Elisp condition (throw with condition data)."
  [condition data]
  (throw (ex-info (str condition ": " (pr-str data))
                  {:condition condition :data data})))

(defn- eval-condition-case [args]
  (let [[var bodyform & handlers] args]
    (try
      (eval bodyform)
      (catch clojure.lang.ExceptionInfo e
        (if (:xmas-throw-tag (ex-data e))
          (throw e)  ;; rethrow catch/throw — don't intercept
          (let [{:keys [condition data]} (ex-data e)
                condition (or condition 'error)
                data (or data (list (.getMessage e)))
                match (some (fn [h] (when (or (= (first h) condition) (= (first h) 'error)) h))
                            handlers)]
            (if match
              (binding [*env* (if var (assoc *env* var (atom (cons condition data))) *env*)]
                (eval-body (rest match)))
              (throw e))))))))

(defn- eval-unwind-protect [[bodyform & cleanup]]
  (try (eval bodyform) (finally (eval-body cleanup))))

(defn- eval-if [[test then & else]]
  (if (eval test) (eval then) (eval-body else)))

(defn- eval-cond [clauses]
  (loop [cs clauses]
    (when (seq cs)
      (let [[test & body] (first cs)]
        (if (or (= test 't) (eval test))
          (eval-body body)
          (recur (rest cs)))))))

(defn- eval-setq [args]
  (reduce (fn [_ [sym val-form]]
            (let [v (eval val-form)]
              (if-let [cell (get *env* sym)]
                (reset! cell v)
                (if (or (contains? (buf-locals) sym)
                        (contains? @*buffer-local-syms* sym))
                  (update-buf! #(assoc-in % [:locals sym] v))
                  (swap! *vars* assoc sym v)))
              v))
          nil (partition 2 args)))

(defn- binding-pair
  "Extract [sym init-form] from a let binding, handling dotted pairs and bare symbols."
  [b]
  (cond
    (symbol? b)              [b nil]
    (instance? DottedPair b) [(.head ^DottedPair b) (.tail ^DottedPair b)]
    :else                    [(first b) (second b)]))

(defn- eval-let [args]
  (let [[bindings & body] args
        cells (make-cells (map (fn [b] (let [[sym expr] (binding-pair b)] [sym (eval expr)])) bindings))]
    (binding [*env* (merge *env* cells)]
      (eval-body body))))

(defn- eval-let* [args]
  (let [[bindings & body] args
        new-env (reduce (fn [env b]
                          (let [[sym expr] (binding-pair b)
                                v (binding [*env* env] (eval expr))]
                            (assoc env sym (atom v))))
                        *env* bindings)]
    (binding [*env* new-env]
      (eval-body body))))

(defn- eval-def* [store [name arglist & body]]
  (let [parsed (parse-arglist arglist)
        [interactive body] (if (and (seq body) (seq? (first body)) (= (ffirst body) 'interactive))
                             [(or (second (first body)) true) (rest body)]
                             [nil body])]
    (swap! store assoc name (cond-> {:args (vec arglist) :parsed parsed :body body :env *env*}
                              interactive (assoc :interactive interactive)))
    name))

(defn- eval-defvar* [[name & rest-args] always?]
  (let [docstring (when (and (next rest-args) (string? (second rest-args)))
                    (second rest-args))]
    (when (and (seq rest-args)
               (or always? (not (contains? @*vars* name))))
      (swap! *vars* assoc name (eval (first rest-args))))
    (when docstring
      (swap! *docs* assoc name docstring))
    name))

(defn- eval-defvar [args] (eval-defvar* args false))
(defn- eval-defconst [args] (eval-defvar* args true))

(defn- eval-while [[test & body]]
  (while (eval test)
    (eval-body body)))

(defn- eval-and [args]
  (reduce (fn [_ form] (let [v (eval form)] (or v (reduced nil)))) true args))

(defn- eval-or [args]
  (reduce (fn [_ form] (let [v (eval form)] (when v (reduced v)))) nil args))

(defn- eval-save-excursion [body]
  (let [{:keys [point mark]} (el-cur)]
    (try
      (eval-body body)
      (finally
        (update-buf! #(assoc % :point point :mark mark))))))

;; --- Backquote ---

(defn- expand-backquote-elems
  "Walk elements of a backquoted collection, handling splice-unquote."
  [form]
  (seq (mapcat (fn [elem]
                 (if (and (seq? elem) (= (first elem) 'splice-unquote))
                   (let [v (eval (second elem))]
                     (when (and v (not (or (seq? v) (sequential? v))))
                       (err (str "Attempt to splice non-list: " (pr-str v))))
                     v)
                   [(expand-backquote elem)]))
               form)))

(defn- expand-backquote
  "Process a backquote template into a value."
  [form]
  (cond
    (and (seq? form) (= (first form) 'unquote)) (eval (second form))
    (seq? form)    (apply list (expand-backquote-elems form))
    (vector? form) (vec (expand-backquote-elems form))
    :else form))

;; --- User function / macro application ---

(defn- apply-user-fn [{:keys [parsed body env]} call-args]
  (let [bindings (bind-args parsed call-args "function")]
    (binding [*env* (merge (or env {}) (make-cells bindings))]
      (eval-body body))))

;; --- Buffer bridge ---

(defn- el-point [] (:point (el-cur)))
(defn- el-point-min [] 0)
(defn- el-point-max [] (count (:text (el-cur))))

(defn- el-goto-char [n]
  (update-buf! #(assoc % :point (max 0 (min n (count (:text %))))))
  n)

(defn- move-n [pos-fn n]
  (dotimes [_ (or n 1)]
    (update-buf! #(buf/set-point % pos-fn))))

(defn- el-forward-char [& [n]] (move-n text/next-pos n))
(defn- el-backward-char [& [n]] (move-n text/prev-pos n))

(defn- el-beginning-of-line []
  (update-buf! #(buf/set-point % text/line-start)))

(defn- el-end-of-line []
  (update-buf! #(buf/set-point % text/line-end)))

(defn- el-insert [& strings]
  (let [text (apply str strings)]
    (update-buf! (fn [b] (buf/edit b (:point b) (:point b) text)))))

(defn- el-delete-region [from to]
  (let [lo (min from to) hi (max from to)]
    (update-buf! #(buf/edit % lo hi ""))))

(defn- el-delete-char [& [n]]
  (dotimes [_ (or n 1)]
    (update-buf! (fn [b]
      (let [p (:point b) t (:text b)]
        (if (< p (count t)) (buf/edit b p (text/next-pos t p) "") b))))))

(defn- el-buffer-string []
  (str (:text (el-cur))))

(defn- el-buffer-name []
  (:name (el-cur)))

(defn- el-search [direction pattern]
  (let [b (el-cur) t (:text b) p (:point b)
        found (if (= direction :forward)
                (text/search-forward t pattern (inc p))
                (text/search-backward t pattern p))]
    (when found (el-goto-char (if (= direction :forward) (+ found (count pattern)) found)))
    found))

(defn- el-message [fmt & args]
  (let [msg (if (seq args)
              (apply format fmt args)
              (str fmt))]
    (swap! *state* assoc :msg msg)
    msg))

;; --- Key string parsing ---

(defn parse-key-string
  "Parse an Emacs key string like \"\\C-x\\C-s\" into a vector of internal key representations."
  [^String s]
  (let [rdr (PushbackReader. (StringReader. s))]
    (loop [keys []]
      (let [ch (read-ch rdr)]
        (cond
          (nil? ch) keys
          (= ch \\)
          (let [mod (read-ch rdr)]
            (if-let [kw ({\C :ctrl \M :meta} mod)]
              (do (read-ch rdr) (recur (conj keys [kw (read-ch rdr)])))
              (recur (conj keys (char mod)))))
          :else (recur (conj keys ch)))))))

;; --- Keybinding bridge ---

(defn- make-key-handler
  "Wrap an Elisp fn spec into a Clojure (state -> state) handler for ed.clj."
  [elisp-fn]
  (let [env (capture-env)]
    (fn [s]
      (with-bindings (assoc env #'*state* (atom s))
        (apply-user-fn elisp-fn [])
        @*state*))))

(def ^:private kbd-special
  {"RET" :return "TAB" :tab "SPC" \space "DEL" :backspace "ESC" :escape
   "return" :return "tab" :tab "backspace" :backspace "escape" :escape
   "delete" :delete "insert" :insert "up" :up "down" :down "left" :left
   "right" :right "home" :home "end" :end "prior" :page-up "next" :page-down
   "f1" :f1 "f2" :f2 "f3" :f3 "f4" :f4})

(defn- parse-kbd-part [^String part]
  (or (kbd-special part)
      (when (and (.startsWith part "<") (> (count part) 1))
        (let [inner (subs part 1 (dec (count part)))]
          (or (kbd-special inner) (keyword inner))))
      (when (and (.startsWith part "C-M-") (> (count part) 4)) [:ctrl [:meta (.charAt part 4)]])
      (when (and (.startsWith part "M-C-") (> (count part) 4)) [:meta [:ctrl (.charAt part 4)]])
      (when (and (.startsWith part "C-") (> (count part) 2))
        (if (> (count part) 3) [:ctrl (parse-kbd-part (subs part 2))] [:ctrl (.charAt part 2)]))
      (when (and (.startsWith part "M-") (> (count part) 2))
        (if (> (count part) 3) [:meta (parse-kbd-part (subs part 2))] [:meta (.charAt part 2)]))
      (when (= 1 (count part)) (.charAt part 0))
      (keyword part)))

(defn- el-kbd [s]
  (let [parts (vec (.split ^String (str s) " +"))]
    (mapv parse-kbd-part parts)))

(defn- el-global-set-key [key-spec func-sym]
  (let [keys (if (vector? key-spec) key-spec (parse-key-string (str key-spec)))
        elisp-fn (or (get @*fns* func-sym)
                     (err (str "Void function: " func-sym)))
        handler (make-key-handler elisp-fn)]
    (if (= 1 (count keys))
      (swap! *vars* update :xmas/bindings (fnil assoc {}) (first keys) handler)
      (swap! *vars* update-in [:xmas/bindings (first keys)] (fnil assoc {}) (second keys) handler))))

;; --- Load system ---

(defn- el-load-file [filename]
  (eval-body (read-all (slurp filename)))
  true)

(defn- el-load [name-or-path]
  (let [name-str (str name-or-path)
        load-path (or (seq (get @*vars* 'load-path))
                      @*load-path*)
        candidates (concat
                     [name-str (str name-str ".el")]
                     (for [dir load-path
                           ext ["" ".el"]]
                       (str dir "/" name-str ext)))]
    (if-let [found (first (filter #(.exists (java.io.File. ^String %)) candidates))]
      (el-load-file found)
      (err (str "Cannot open load file: " name-str)))))

(defn- el-provide [feature]
  (swap! *features* conj feature)
  feature)

(defn- el-featurep [feature]
  (contains? @*features* feature))

(defn- el-require [feature]
  (when-not (or (contains? @*features* feature)
                (contains? @*loading* feature))
    (swap! *loading* conj feature)
    (try
      (el-load feature)
      (finally
        (swap! *loading* disj feature))))
  feature)

;; --- Property lists ---

(defn- el-put [sym prop val]
  (swap! *plists* assoc-in [sym prop] val)
  val)

(defn- el-get [sym prop]
  (get-in @*plists* [sym prop]))

(defn- el-plist-get [plist prop]
  (some (fn [[k v]] (when (= k prop) v)) (partition 2 plist)))

(defn- el-plist-put [plist prop val]
  (let [pairs (partition 2 plist)
        found (some #(= (first %) prop) pairs)]
    (apply list (mapcat (fn [[k v]] [k (if (= k prop) val v)])
                        (if found pairs (concat pairs [[prop val]]))))))

;; --- Buffer-local variables ---

(defn- el-make-local-variable [sym]
  (update-buf! (fn [b]
    (if (contains? (:locals b) sym) b
      (assoc-in b [:locals sym] (get @*vars* sym)))))
  sym)

(defn- el-make-variable-buffer-local [sym]
  (swap! *buffer-local-syms* conj sym)
  sym)

(defn- el-buffer-local-value [sym buf-name]
  (let [b (get (:bufs @*state*) (str buf-name))]
    (get (:locals b) sym (get @*vars* sym))))

(defn- el-local-variable-p [sym]
  (contains? (buf-locals) sym))

;; --- Hooks ---

(defn- el-add-hook [hook fn-sym & [append]]
  (swap! *vars* update hook
    (fn [fns] (let [fns (or fns ())]
                (if append (concat fns (list fn-sym)) (cons fn-sym fns))))))

(defn- el-remove-hook [hook fn-sym]
  (swap! *vars* update hook (fn [fns] (remove #(= % fn-sym) fns))))

(defn- hook-fns [hook]
  (let [v (var-lookup hook)]
    (when-not (= v ::not-found) v)))

(defn- el-run-hooks [& hooks]
  (doseq [h hooks, fn-sym (hook-fns h)]
    (call-fn fn-sym [])))

(defn- el-run-hook-with-args [hook & args]
  (doseq [fn-sym (hook-fns hook)]
    (call-fn fn-sym args)))

;; --- Keymaps ---

(defn- el-make-sparse-keymap []
  {:xmas/keymap true :bindings {} :parent nil})

(defn- keymap? [x]
  (and (map? x) (:xmas/keymap x)))

(defn- el-define-key [keymap key def]
  (assoc-in keymap [:bindings key] def))

(defn- el-set-keymap-parent [keymap parent]
  (assoc keymap :parent parent))

(defn- keymap-lookup [keymap key]
  (when keymap
    (or (get (:bindings keymap) key)
        (keymap-lookup (:parent keymap) key))))

(defn- el-current-local-map []
  (get (buf-locals) :xmas/local-keymap))

(defn- el-use-local-map [keymap]
  (update-buf! #(assoc-in % [:locals :xmas/local-keymap] keymap)))

(defn- el-current-global-map []
  (get @*vars* :xmas/global-keymap))

;; --- Regular expressions ---

(def ^:private esc-remap
  {\( "(" \) ")" \| "|" \< "\\b" \> "\\b" \` "\\A" \' "\\z"})

(def ^:private esc-passthrough
  #{\w \W \b \B \s \S})

(defn- emacs->java-regex
  "Translate Emacs regex to Java regex. Key difference: group delimiters are swapped."
  [^String pat]
  (let [sb (StringBuilder.) len (count pat)]
    (loop [i 0]
      (if (>= i len) (str sb)
        (let [ch (.charAt pat i)]
          (if (and (= ch \\) (< (inc i) len))
            (let [nx (.charAt pat (inc i))]
              (if-let [repl (esc-remap nx)]
                (do (.append sb ^String repl) (recur (+ i 2)))
                (do (when (or (esc-passthrough nx) (<= (int \1) (int nx) (int \9)))
                      (.append sb "\\"))
                    (.append sb nx) (recur (+ i 2)))))
            (do (case ch \( (.append sb "\\(") \) (.append sb "\\)") (.append sb ch))
                (recur (inc i)))))))))
(defn- save-match-data!
  "Store match groups from a Matcher, offsetting positions by `off`."
  [^java.util.regex.Matcher m ^long off]
  (let [gc (.groupCount m)]
    (el-var! :xmas/match-data
      (vec (for [g (range (inc gc))]
             [(+ off (.start m (int g))) (+ off (.end m (int g)))])))))

(defn- do-match [^String text ^String emacs-pat ^long start]
  (let [jpat (emacs->java-regex emacs-pat)
        m (.matcher (Pattern/compile jpat) text)]
    (when (.find m (int start))
      (save-match-data! m 0)
      (.start m))))

(defn- el-string-match [regexp string & [start]]
  (do-match string regexp (or start 0)))

(defn- el-match-string [n & [string]]
  (when-let [md (el-var :xmas/match-data)]
    (when-let [[s e] (get md n)]
      (when (and (>= s 0) (>= e 0))
        (let [src (or string (:text (el-cur)))]
          (subs src s e))))))

(defn- match-pos [n idx]
  (when-let [pair (get (el-var :xmas/match-data) n)]
    (let [v (nth pair idx)] (when (>= v 0) v))))

(defn- el-match-beginning [n] (match-pos n 0))
(defn- el-match-end [n] (match-pos n 1))

(defn- el-replace-regexp-in-string [regexp rep string]
  (let [jpat (emacs->java-regex regexp)]
    (.replaceAll (re-matcher (Pattern/compile jpat) string) ^String rep)))

(defn- el-looking-at [regexp]
  (let [b (el-cur) t (:text b) p (:point b)
        m (.matcher (Pattern/compile (emacs->java-regex regexp)) (subs t p))]
    (when (.lookingAt m)
      (save-match-data! m p)
      true)))

(defn- el-re-search [direction regexp & [bound]]
  (let [b (el-cur) t (:text b) p (:point b)
        backward? (= direction :backward)
        [region start] (if backward?
                         [(subs t (min (count t) (or bound 0)) p) (min (count t) (or bound 0))]
                         [(subs t p (min (count t) (or bound (count t)))) p])
        m (.matcher (Pattern/compile (emacs->java-regex regexp)) region)]
    (when (.find m)
      (let [last-start (if backward?
                         (loop [s (.start m)]
                           (if (.find m) (recur (.start m)) s))
                         (.start m))]
        (when backward? (.find (.reset m) (int last-start)))
        (save-match-data! m start)
        (el-goto-char (+ start (.end m 0)))
        (+ start (.start m 0))))))


(defn- el-match-data []
  (when-let [md (el-var :xmas/match-data)]
    (apply list (mapcat identity md))))

;; --- Text properties ---

(defn- props-at
  "Get merged properties at pos from sorted props map."
  [props pos]
  (when-let [entry (first (rsubseq props <= pos))]
    (val entry)))

(defn- el-put-text-property [start end prop val]
  (update-buf! (fn [b]
    (let [props (:props b)
          before (props-at props start)
          after  (props-at props end)]
      (assoc b :props
        (-> (reduce (fn [p [k _]] (if (<= start k end) (dissoc p k) p)) props props)
            (assoc start (assoc (or before {}) prop val))
            (assoc end (or after {}))))))))

(defn- el-get-text-property [pos prop]
  (let [pmap (props-at (:props (el-cur)) pos)]
    (get pmap prop)))

(defn- el-remove-text-properties [start end prop-list]
  (let [prop-keys (set (take-nth 2 prop-list))]
    (update-buf! (fn [b]
      (let [props (:props b)]
        (assoc b :props
          (reduce (fn [p [k pmap]]
                    (if (<= start k (dec end))
                      (let [cleaned (apply dissoc pmap prop-keys)]
                        (if (empty? cleaned) (dissoc p k) (assoc p k cleaned)))
                      p))
                  props props)))))))

(defn- el-propertize [string & props]
  (let [pmap (apply hash-map props)]
    (swap! *vars* assoc :xmas/propertize-props pmap)
    string))

(defn- el-text-properties-at [pos]
  (when-let [pmap (props-at (:props (el-cur)) pos)]
    (apply list (mapcat identity pmap))))

(defn- el-next-property-change [pos]
  (let [props (:props (el-cur))]
    (when-let [entry (first (subseq props > pos))]
      (key entry))))

;; --- Macro expansion ---

(defn- el-macroexpand [form]
  (if-let [macro (and (seq? form) (symbol? (first form)) (get @*macros* (first form)))]
    (apply-user-fn macro (rest form))
    form))

;; --- Resolve and call functions ---

(declare builtins)

(defn- resolve-fn [sym]
  (let [f (or (get @*fns* sym) (get builtins sym))]
    (cond
      (nil? f)      (err (str "Void function: " sym))
      (:autoload f) (do (el-load (:autoload f))
                        (or (get @*fns* sym) (err (str "Autoload failed: " sym))))
      :else         f)))

(defn- call-fn
  "Resolve f (symbol, lambda map, or builtin) and call with args."
  [f args]
  (let [actual-f (if (symbol? f) (resolve-fn f) f)]
    (if (map? actual-f)
      (apply-user-fn actual-f (vec args))
      (clojure.core/apply actual-f args))))

;; --- Hash tables ---

(defrecord ElHashTable [data test])

(defn- ht-data ^clojure.lang.Atom [table] (.data ^ElHashTable table))

(defn- el-make-hash-table [& args]
  (let [opts (apply hash-map args)
        test (or (get opts ':test) 'equal)]
    (->ElHashTable (atom {}) test)))

(defn- el-gethash [key table & [default]] (get @(ht-data table) key default))
(defn- el-puthash [key value table] (swap! (ht-data table) assoc key value) value)
(defn- el-remhash [key table] (swap! (ht-data table) dissoc key) nil)

(defn- el-maphash [f table]
  (doseq [[k v] @(ht-data table)] (call-fn f [k v])))

(defn- el-hash-table-count [table] (count @(ht-data table)))
(defn- el-hash-table-p [obj] (instance? ElHashTable obj))
(defn- el-hash-table-keys [table] (apply list (keys @(ht-data table))))
(defn- el-hash-table-values [table] (apply list (vals @(ht-data table))))

;; --- Interactive input ---

(defn- mini-read
  "Inline terminal prompt. Returns user input string."
  [prompt]
  (let [rows (:rows @*state* 24) row (dec rows) col0 (count prompt)]
    (t/move row 0) (t/clreol) (t/tw prompt) (t/show-cur) (t/flush!)
    (loop [sb (StringBuilder.) col col0]
      (let [key (t/read-key-timeout 60000)]
        (cond
          (or (nil? key) (= key :return))
          (do (t/move row 0) (t/clreol) (t/flush!) (str sb))
          (= key :backspace)
          (if (pos? (.length sb))
            (do (.deleteCharAt sb (dec (.length sb)))
                (t/move row (dec col)) (t/tw " ") (t/move row (dec col)) (t/flush!)
                (recur sb (dec col)))
            (recur sb col))
          (= key [:ctrl \g])
          (do (t/move row 0) (t/clreol) (t/flush!)
              (el-signal 'quit (list "Quit")))
          (and (char? key) (>= (int key) 32))
          (do (.append sb key) (t/tw (str key)) (t/flush!)
              (recur sb (inc col)))
          :else (recur sb col))))))

(defn- parse-interactive-spec [^String spec]
  (if (empty? spec) []
    (into []
      (mapcat (fn [^String part]
                (when (seq part)
                  (let [code (.charAt part 0)
                        prompt (subs part 1)]
                    (case code
                      \s [(mini-read (if (seq prompt) prompt "String: "))]
                      \b [(mini-read (if (seq prompt) prompt "Buffer: "))]
                      \f [(mini-read (if (seq prompt) prompt "File: "))]
                      \r (let [p (el-point) m (or (:mark (el-cur)) p)]
                           [(min p m) (max p m)])
                      \p [(or (get @*vars* 'current-prefix-arg) 1)]
                      \P [(get @*vars* 'current-prefix-arg)]
                      [nil])))))
      (vec (.split spec "\n")))))

(defn- el-read-from-minibuffer [prompt & _] (mini-read prompt))
(defn- el-completing-read [prompt _collection & _] (mini-read prompt))

(defn- el-call-interactively [func]
  (let [f (if (symbol? func) (resolve-fn func) func)
        spec (when (map? f) (:interactive f))
        args (cond (or (nil? spec) (= spec true)) []
                   (string? spec) (parse-interactive-spec spec)
                   :else [])]
    (binding [*interactive* true]
      (call-fn f args))))

;; --- Autoload ---

(defn- el-autoload [sym file & _]
  (swap! *fns* assoc sym {:autoload (str file)}) nil)

;; --- Set match data ---

(defn- el-set-match-data [data]
  (el-var! :xmas/match-data (vec (map vec (partition 2 data)))))

;; --- Timers ---

(def ^:private el-timers (atom []))
(def ^:private timer-id (atom 0))

(defn- make-timer [fn-sym args delay-ms repeat-ms]
  {:xmas/timer true :id (swap! timer-id inc) :fn-sym fn-sym :args (vec args)
   :delay-ms delay-ms :repeat-ms repeat-ms
   :next-fire (+ (System/currentTimeMillis) delay-ms)})

(defn- el-run-with-timer [secs repeat func & args]
  (let [t (make-timer func args (long (* (or secs 0) 1000))
                      (when repeat (long (* repeat 1000))))]
    (swap! el-timers conj t) t))

(defn- el-cancel-timer [timer]
  (swap! el-timers (fn [ts] (filterv #(not= (:id %) (:id timer)) ts))) nil)

(defn- el-timerp [obj] (and (map? obj) (:xmas/timer obj)))

;; --- Async processes ---

(def ^:private el-processes (atom {}))

(defn- try-eval-callback [editor-atom cb-atom args]
  (when-let [f @cb-atom]
    (try (eval-with-env editor-atom #(call-fn f args))
         (catch Exception _))))

(defn- el-start-process [name buffer-name program & args]
  (let [real-editor (or (:self @*state*) *state*)
        pb (ProcessBuilder. ^java.util.List (into [(str program)] (map str args)))
        proc (.start pb)
        status (atom :run)
        filter-fn (atom nil)
        sentinel-fn (atom nil)
        process {:xmas/process true :name name :buffer buffer-name
                 :proc proc :filter filter-fn :sentinel sentinel-fn :status status}]
    (future
      (try
        (with-open [rdr (BufferedReader. (InputStreamReader. (.getInputStream proc)))]
          (loop []
            (when-let [line (.readLine rdr)]
              (try-eval-callback real-editor filter-fn [process (str line "\n")])
              (recur))))
        (finally
          (reset! status :exit)
          (try-eval-callback real-editor sentinel-fn [process "finished\n"]))))
    (swap! el-processes assoc name process)
    process))

(defn- el-call-process [program & args]
  (let [dest (second args)
        prog-args (drop 3 args)
        pb (doto (ProcessBuilder. ^java.util.List (into [(str program)] (map str prog-args)))
             (.redirectErrorStream true))
        proc (.start pb)
        output (slurp (.getInputStream proc))
        exit (.waitFor proc)]
    (when (= dest true) (el-insert output))
    exit))

(defn- el-process-send-string [process string]
  (let [os (.getOutputStream ^Process (:proc process))]
    (.write os (.getBytes ^String string)) (.flush os)))

(defn- el-set-process-filter [process f] (reset! (:filter process) f))
(defn- el-set-process-sentinel [process f] (reset! (:sentinel process) f))
(defn- el-process-buffer [process] (:buffer process))
(defn- el-process-status [process] @(:status process))

(defn- el-delete-process [process]
  (.destroy ^Process (:proc process))
  (swap! el-processes dissoc (:name process)) nil)

(defn- el-shell-command-to-string [command]
  (let [proc (.start (doto (ProcessBuilder. ^java.util.List ["sh" "-c" command])
                       (.redirectErrorStream true)))
        output (slurp (.getInputStream proc))]
    (.waitFor proc) output))

;; --- Additional C-primitive helpers ---

(defn- el-type-of [x]
  (cond (nil? x) 'symbol (boolean? x) 'symbol (integer? x) 'integer
        (instance? Double x) 'float (string? x) 'string (symbol? x) 'symbol
        (char? x) 'integer (seq? x) 'cons (vector? x) 'vector
        (instance? ElHashTable x) 'hash-table (fn? x) 'subr
        (and (map? x) (:body x)) 'closure :else 'subr))

(defn- el-prin1-to-string [obj & _]
  (cond
    (nil? obj)     "nil"
    (= obj true)   "t"
    (= obj false)  "nil"
    (string? obj)   (str "\"" (-> ^String obj (.replace "\\" "\\\\") (.replace "\"" "\\\"")
                                  (.replace "\n" "\\n") (.replace "\t" "\\t")) "\"")
    (symbol? obj)   (name obj)
    (char? obj)     (str "?" obj)
    (instance? DottedPair obj) (str "(" (el-prin1-to-string (.head ^DottedPair obj))
                                    " . " (el-prin1-to-string (.tail ^DottedPair obj)) ")")
    (vector? obj)   (str "[" (str/join " " (map el-prin1-to-string obj)) "]")
    (seq? obj)      (str "(" (str/join " " (map el-prin1-to-string obj)) ")")
    :else           (str obj)))

(defn- el-princ-to-string [obj]
  (if (string? obj) obj (el-prin1-to-string obj)))

;; Buffer position helpers
(defn- el-bobp [] (zero? (el-point)))
(defn- el-eobp [] (= (el-point) (el-point-max)))
(defn- el-bolp [] (let [p (el-point) t (:text (el-cur))]
                    (or (zero? p) (= (.charAt ^String t (dec p)) \newline))))
(defn- el-eolp [] (let [p (el-point) t (:text (el-cur))]
                    (or (= p (count t)) (= (.charAt ^String t p) \newline))))
(defn- el-char-after [& [pos]]
  (let [p (or pos (el-point)) t (:text (el-cur))]
    (when (< p (count t)) (int (.charAt ^String t (int p))))))
(defn- el-char-before [& [pos]]
  (let [p (or pos (el-point)) t (:text (el-cur))]
    (when (pos? p) (int (.charAt ^String t (int (dec p)))))))
(defn- el-buffer-size [] (count (:text (el-cur))))
(defn- el-buffer-substring [start end]
  (subs (:text (el-cur)) (min start end) (max start end)))
(defn- el-erase-buffer []
  (update-buf! (fn [b] (assoc b :text "" :point 0 :mark nil))))
(defn- el-line-pos [f] (f (:text (el-cur)) (el-point)))
(defn- el-forward-line [& [n]]
  (let [n (or n 1)
        step (if (pos? n)
               #(let [eol (text/line-end (:text %) (:point %))]
                  (assoc % :point (min (inc eol) (count (:text %)))))
               #(let [ls (text/line-start (:text %) (:point %))]
                  (assoc % :point (max 0 (dec ls)))))]
    (dotimes [_ (Math/abs (int n))] (update-buf! step))
    0))

;; Buffer management helpers
(defn- el-current-buffer [] (:buf @*state*))
(defn- el-set-buffer [name]
  (let [name (str name)]
    (when-not (get (:bufs @*state*) name) (err (str "No buffer named " name)))
    (swap! *state* assoc :buf name) name))
(defn- el-get-buffer [name]
  (let [name (str name)] (when (get (:bufs @*state*) name) name)))
(defn- el-get-buffer-create [name]
  (let [name (str name)]
    (when-not (get (:bufs @*state*) name)
      (swap! *state* assoc-in [:bufs name] (buf/make name)))
    name))
(defn- el-buffer-list [] (apply list (keys (:bufs @*state*))))
(defn- el-kill-buffer [& [name]]
  (let [name (str (or name (:buf @*state*)))]
    (swap! *state* (fn [s]
      (let [s (update s :bufs dissoc name)]
        (cond-> s (= (:buf s) name)
          (assoc :buf (or (first (keys (:bufs s))) "*scratch*"))))))
    true))
(defn- el-buffer-file-name [& _] (:file (el-cur)))
(defn- el-buffer-modified-p [& _] (:modified (el-cur)))
(defn- el-buffer-live-p [name] (boolean (get (:bufs @*state*) (str name))))
(defn- el-generate-new-buffer-name [name]
  (let [name (str name) bufs (:bufs @*state*)]
    (if-not (get bufs name) name
      (loop [i 2] (let [n (str name "<" i ">")] (if-not (get bufs n) n (recur (inc i))))))))
(defn- el-other-buffer [& _]
  (let [cur (:buf @*state*)]
    (or (first (remove #{cur} (keys (:bufs @*state*)))) cur)))

;; File helpers
(defn- el-expand-file-name [name & [dir]]
  (let [dir (or dir (System/getProperty "user.dir"))
        name (str name)
        name (if (.startsWith name "~")
               (str (System/getProperty "user.home") (subs name 1))
               name)]
    (.getCanonicalPath (java.io.File. (str dir) name))))

(defn- el-insert-file-contents [filename & _]
  (let [content (slurp (str filename))]
    (el-insert content)
    (list (str filename) (count content))))

(defn- el-write-region [start end filename & _]
  (let [content (if (and start end)
                  (subs (:text (el-cur)) (min start end) (max start end))
                  (:text (el-cur)))]
    (spit (str filename) content)
    nil))

;; Sort/map helpers (need call-fn)
(defn- el-sort [lst pred]
  (apply list (sort (fn [a b] (boolean (call-fn pred [a b]))) lst)))
(defn- el-mapc [f lst] (doseq [x lst] (call-fn f [x])) lst)
(defn- el-mapcan [f lst] (apply list (mapcat #(call-fn f [%]) lst)))
(defn- el-mapconcat [f lst sep]
  (str/join (str sep) (map #(str (call-fn f [%])) lst)))

;; Catch/throw
(defn- eval-catch [[tag-form & body]]
  (let [tag (eval tag-form)]
    (try (eval-body body)
      (catch clojure.lang.ExceptionInfo e
        (if (= (:xmas-throw-tag (ex-data e)) tag)
          (:xmas-throw-value (ex-data e))
          (throw e))))))
(defn- el-throw [tag value]
  (throw (ex-info (str "throw: " tag) {:xmas-throw-tag tag :xmas-throw-value value})))

;; --- Shared builtin helpers ---

(def ^:private noop (fn [& _] nil))
(def ^:private stub-t (fn [& _] true))
(defn- ->file ^java.io.File [p] (java.io.File. (str p)))
(defn- file-check [f] (fn [p] (f (->file p))))
(defn- case-convert [char-fn str-fn]
  (fn [x] (if (char? x) (char (char-fn (int x))) (str-fn (str x)))))
(defn- sys-prop [k] (fn [& _] (System/getProperty k)))
(defn- div-opt ^double [x d] (if d (/ (double x) (double d)) (double x)))
(defn- el-bol-pos    [& _] (el-line-pos text/line-start))
(defn- el-eol-pos    [& _] (el-line-pos text/line-end))
(defn- el-switch-to  [buf & _] (el-set-buffer (str buf)))
(defn- el-win-cols   [& _] (:cols @*state* 80))
(defn- el-win-rows   [& _] (:rows @*state* 24))
(defn- dispatch-dotted [dot-fn seq-fn]
  (fn [x] (cond (nil? x) nil (instance? DottedPair x) (dot-fn x) :else (seq-fn x))))
(defn- remove-by [pred] (fn [elt lst] (apply list (clojure.core/remove #(pred elt %) lst))))
(defn- search-list [pred] (fn [elt lst] (loop [l (seq lst)] (when l (if (pred elt (first l)) l (recur (next l)))))))
(defn- str= [a b] (= (str a) (str b)))
(defn- str< [a b] (neg? (.compareTo ^String (str a) ^String (str b))))

;; --- Data-driven stubs ---

(def ^:private noop-syms
  '[bool-vector-p char-table-p bufferp markerp recordp
    file-attributes find-file-name-handler setenv special-variable-p
    backtrace-frame--internal mapbacktrace ding minibufferp])

(def ^:private stub-t-syms
  '[multibyte-string-p frame-live-p window-live-p])

(def ^:private identity-syms
  '[copy-sequence copy-alist string-to-multibyte string-to-unibyte
    string-as-multibyte string-as-unibyte string-make-multibyte
    string-make-unibyte unibyte-char-to-multibyte multibyte-char-to-unibyte])

;; --- Built-in function table ---

(def ^:private builtins
  (merge
    (zipmap noop-syms (repeat noop))
    (zipmap stub-t-syms (repeat stub-t))
    (zipmap identity-syms (repeat identity))
  {;; arithmetic
   '+       +,       '-       -,       '*       *
   '/       /,       'mod     mod
   '<       <,       '>       >,       '=       ==
   '<=      <=,      '>=      >=
   (symbol "1+") inc
   (symbol "1-") dec
   ;; list
   'cons    (fn [a b] (if (or (nil? b) (seq? b)) (clojure.core/cons a b) (DottedPair. a b)))
   'car     (dispatch-dotted #(.head ^DottedPair %) first)
   'cdr     (dispatch-dotted #(.tail ^DottedPair %) next)
   'list    list,    'length  count
   'nth     (fn [n l] (nth l n))
   'null    nil?,    'append  concat
   'reverse clojure.core/reverse
   ;; string
   'concat  str
   'substring (fn
                ([s from] (subs s from))
                ([s from to] (subs s from to)))
   'format  format
   'string-to-number (fn [s] (or (try (Long/parseLong s) (catch Exception _ nil))
                                  (try (Double/parseDouble s) (catch Exception _ nil))
                                  0))
   'number-to-string str
   'char-to-string   str
   ;; predicates
   'numberp  number?,  'stringp  string?
   'listp    (fn [x] (or (nil? x) (seq? x) (instance? DottedPair x)))
   'consp    (fn [x] (or (seq? x) (instance? DottedPair x)))
   'symbolp  symbol?
   'equal    =,        'not      not
   'boundp   (fn [sym] (not= (var-lookup sym) ::not-found))
   'fboundp  (fn [sym] (or (contains? @*fns* sym)
                            (contains? builtins sym)
                            (contains? @*macros* sym)))
   'functionp (fn [x] (or (fn? x) (and (map? x) (contains? x :body))))
   ;; symbols
   'intern   symbol
   ;; higher-order
   'apply    (fn [f & args]
               (call-fn f (if (seq args) (concat (butlast args) (last args)) ())))
   'funcall  (fn [f & args] (call-fn f args))
   'mapcar   (fn [f lst] (apply list (map #(call-fn f [%]) lst)))
   ;; error
   'error    (fn [fmt & args]
               (el-signal 'error (list (if (seq args) (clojure.core/apply format fmt args) (str fmt)))))
   'signal   el-signal
   ;; buffer
   'point             el-point
   'point-min         el-point-min
   'point-max         el-point-max
   'goto-char         el-goto-char
   'forward-char      el-forward-char
   'backward-char     el-backward-char
   'beginning-of-line el-beginning-of-line
   'end-of-line       el-end-of-line
   'insert            el-insert
   'delete-region     el-delete-region
   'delete-char       el-delete-char
   'buffer-string     el-buffer-string
   'buffer-name       el-buffer-name
   'search-forward    (fn [pat] (el-search :forward pat))
   'search-backward   (fn [pat] (el-search :backward pat))
   'message           el-message
   'global-set-key    el-global-set-key
   'kbd               el-kbd
   'local-set-key (fn [key def]
                              (let [km (or (el-current-local-map) (el-make-sparse-keymap))]
                                (el-use-local-map (el-define-key km (if (vector? key) (first key) key) def))))
   ;; load system
   'provide           el-provide
   'require           el-require
   'load              el-load
   'load-file el-load-file
   'featurep          el-featurep
   ;; property lists
   'put               el-put
   'get               el-get
   'plist-get         el-plist-get
   'plist-put         el-plist-put
   ;; macros
   'macroexpand       el-macroexpand
   'eval              (fn [form & _] (eval form))
   ;; buffer-local variables
   'make-local-variable          el-make-local-variable
   'make-variable-buffer-local   el-make-variable-buffer-local
   'buffer-local-value           el-buffer-local-value
   'local-variable-p             el-local-variable-p
   ;; hooks
   'add-hook             el-add-hook
   'remove-hook          el-remove-hook
   'run-hooks            el-run-hooks
   'run-hook-with-args   el-run-hook-with-args
   ;; symbols
   'symbol-name  (fn [sym] (clojure.core/name sym))
   ;; keymaps
   'make-sparse-keymap    el-make-sparse-keymap
   'define-key            el-define-key
   'set-keymap-parent     el-set-keymap-parent
   'lookup-key            keymap-lookup
   'current-local-map     el-current-local-map
   'current-global-map    el-current-global-map
   'use-local-map         el-use-local-map
   'keymapp                         keymap?
   'commandp   (fn [sym] (boolean (:interactive (get @*fns* sym))))
   ;; regex
   'string-match               el-string-match
   'match-string               el-match-string
   'match-beginning            el-match-beginning
   'match-end                  el-match-end
   'replace-regexp-in-string   el-replace-regexp-in-string
   'looking-at                 el-looking-at
   're-search-forward          (fn [regexp & [bound]] (el-re-search :forward regexp bound))
   're-search-backward         (fn [regexp & [bound]] (el-re-search :backward regexp bound))
   'match-data                 el-match-data
   ;; text properties
   'put-text-property          el-put-text-property
   'get-text-property          el-get-text-property
   'remove-text-properties     el-remove-text-properties
   'propertize                           el-propertize
   'text-properties-at         el-text-properties-at
   'next-property-change       el-next-property-change
   ;; interactive
   'interactive-p              (fn [] *interactive*)
   'called-interactively-p     (fn [& _] *interactive*)
   'call-interactively         el-call-interactively
   'read-string               mini-read
   'read-from-minibuffer       el-read-from-minibuffer
   'completing-read            el-completing-read
   ;; hash tables
   'make-hash-table            el-make-hash-table
   'gethash                              el-gethash
   'puthash                              el-puthash
   'remhash                              el-remhash
   'maphash                              el-maphash
   'hash-table-count           el-hash-table-count
   'hash-table-p               el-hash-table-p
   'hash-table-keys            el-hash-table-keys
   'hash-table-values          el-hash-table-values
   ;; autoload
   'autoload                             el-autoload
   ;; match data
   'set-match-data             el-set-match-data
   ;; timers
   'run-with-timer             el-run-with-timer
   'run-with-idle-timer        el-run-with-timer
   'cancel-timer               el-cancel-timer
   'timerp                               el-timerp
   ;; processes
   'start-process              el-start-process
   'call-process               el-call-process
   'process-send-string        el-process-send-string
   'set-process-filter         el-set-process-filter
   'set-process-sentinel       el-set-process-sentinel
   'process-buffer             el-process-buffer
   'process-status             el-process-status
   'delete-process             el-delete-process
   'shell-command-to-string    el-shell-command-to-string
   ;; ============================================================
   ;; DIRECT-MAPPING C PRIMITIVES
   ;; ============================================================
   ;; --- predicates ---
   'eq             (fn [a b] (if (and (symbol? a) (symbol? b)) (= a b) (identical? a b)))
   'eql            =
   'integerp       integer?
   'floatp         (fn [x] (instance? Double x))
   'characterp     char?
   'vectorp        vector?
   'zerop          zero?
   'natnump        (fn [x] (and (integer? x) (>= x 0)))
   'atom           (fn [x] (not (or (seq? x) (instance? DottedPair x))))
   'nlistp         (fn [x] (and (not (nil? x)) (not (seq? x))))
   'sequencep      (fn [x] (or (nil? x) (seq? x) (string? x) (vector? x)))
   'arrayp         (fn [x] (or (string? x) (vector? x)))
   'subrp          fn?
   'keywordp       (fn [x] (and (symbol? x) (.startsWith ^String (name x) ":")))
   'char-or-string-p   (fn [x] (or (char? x) (string? x)))
   'number-or-marker-p number?
   'integer-or-marker-p integer?
   'proper-list-p      (fn [x] (or (nil? x) (seq? x)))
   'processp       (fn [x] (and (map? x) (:xmas/process x)))
   'type-of       el-type-of
   'cl-type-of    el-type-of
   ;; --- immutability freebies ---
   'nreverse                   clojure.core/reverse
   'nconc                      concat
   'ntake                      (fn [n l] (take n l))
   'safe-length      count
   'car-safe         (fn [x] (when (seq? x) (first x)))
   'cdr-safe         (fn [x] (when (seq? x) (next x)))
   'substring-no-properties (fn ([s from] (subs s from)) ([s from to] (subs s from to)))
   'clear-string        (fn [_] nil)
   ;; --- math (Java Math) ---
   'abs       abs
   'max       max
   'min       min
   'float     double
   'floor     (fn [x & [d]] (long (Math/floor (div-opt x d))))
   'ceiling   (fn [x & [d]] (long (Math/ceil  (div-opt x d))))
   'round     (fn [x & [d]] (Math/round (div-opt x d)))
   'truncate  (fn [x & [d]] (long (div-opt x d)))
   'expt      (fn [a b] (Math/pow a b))
   'sqrt      (fn [x] (Math/sqrt x))
   'sin       (fn [x] (Math/sin x))
   'cos       (fn [x] (Math/cos x))
   'tan       (fn [x] (Math/tan x))
   'asin      (fn [x] (Math/asin x))
   'acos      (fn [x] (Math/acos x))
   'atan      (fn [x & [y]] (if y (Math/atan2 x y) (Math/atan x)))
   'exp       (fn [x] (Math/exp x))
   'log       (fn [x & [base]] (if base (/ (Math/log x) (Math/log base)) (Math/log x)))
   'random    (fn [& [n]] (if n (rand-int n) (rand-int Integer/MAX_VALUE)))
   'isnan     (fn [x] (Double/isNaN (double x)))
   'ffloor    (fn [x] (Math/floor x))
   'fceiling  (fn [x] (Math/ceil x))
   'fround    (fn [x] (Math/rint x))
   'ftruncate (fn [x] (double (long x)))
   'copysign  (fn [x y] (Math/copySign (double x) (double y)))
   'ldexp     (fn [x e] (* x (Math/pow 2 e)))
   'logb      (fn [x] (if (zero? x) Long/MIN_VALUE
                        (long (/ (Math/log (Math/abs (double x))) (Math/log 2)))))
   ;; --- bitwise ---
   'logand    bit-and
   'logior    bit-or
   'logxor    bit-xor
   'lognot    bit-not
   'ash       (fn [n c] (if (>= c 0) (bit-shift-left n c) (bit-shift-right n (- c))))
   'logcount  (fn [n] (Long/bitCount n))
   ;; --- string (Java String) ---
   'upcase    (case-convert #(Character/toUpperCase %) #(.toUpperCase ^String %))
   'downcase  (case-convert #(Character/toLowerCase %) #(.toLowerCase ^String %))
   'capitalize (fn [s] (let [s (str s)] (case (count s)
                  0 s, 1 (str (Character/toUpperCase (.charAt s 0)))
                  (str (Character/toUpperCase (.charAt s 0)) (.toLowerCase (.substring s 1))))))
   'string-equal   str=
   'string=        str=
   'string-lessp   str<
   'string<        str<
   'string-search  (fn [needle haystack & [start]]
                               (let [i (.indexOf ^String (str haystack) ^String (str needle)
                                                  (int (or start 0)))]
                                 (when (>= i 0) i)))
   'string-prefix-p (fn [prefix s & _]
                                (.startsWith ^String (str s) ^String (str prefix)))
   'string-suffix-p (fn [suffix s & _]
                                (.endsWith ^String (str s) ^String (str suffix)))
   'string-to-char  (fn [s] (let [s (str s)] (when (seq s) (int (.charAt s 0)))))
   'make-string     (fn [n ch] (apply str (repeat n (if (integer? ch) (char ch) ch))))
   'regexp-quote    (fn [s] (java.util.regex.Pattern/quote (str s)))
   'string-bytes    (fn [s] (count (.getBytes ^String (str s) "UTF-8")))
   'string-width    (fn [s] (count (str s)))
   'char-width      (fn [_] 1)
   'char-equal      (fn [a b] (= (int a) (int b)))
   'max-char        (constantly 0x10FFFF)
   'byte-to-string  (fn [b] (str (char b)))
   'string-distance (fn [a b & _] (let [a (str a) b (str b) la (count a) lb (count b)
                                 d (to-array-2d (repeat (inc la) (vec (range (inc lb)))))]
                                 (doseq [i (range 1 (inc la)) j (range 1 (inc lb))]
                                   (aset d i j (if (= (.charAt a (dec i)) (.charAt b (dec j)))
                                                 (aget d (dec i) (dec j))
                                                 (inc (min (aget d (dec i) j)
                                                           (aget d i (dec j))
                                                           (aget d (dec i) (dec j)))))))
                                 (aget d la lb)))
   'compare-strings (fn [s1 start1 end1 s2 start2 end2 & [ignore-case]]
                                (let [a (subs (str s1) (or start1 0) (or end1 (count (str s1))))
                                      b (subs (str s2) (or start2 0) (or end2 (count (str s2))))
                                      cmp (if ignore-case
                                            (.compareToIgnoreCase ^String a b)
                                            (.compareTo ^String a b))]
                                  (cond (zero? cmp) true (neg? cmp) (- (inc (count a))) :else (inc (count a)))))
   'split-string    (fn [s & [sep omit-nulls]]
                                (let [parts (if sep
                                              (vec (.split ^String (str s) (str sep) -1))
                                              (vec (.split ^String (str s) "[ \\t\\n\\r]+" -1)))]
                                  (apply list (if omit-nulls (filterv seq parts) parts))))
   'string-match-p  (fn [regexp string & [start]]
                                (let [jpat (emacs->java-regex regexp)
                                      m (.matcher (java.util.regex.Pattern/compile jpat)
                                                  ^String (str string))]
                                  (when (.find m (int (or start 0))) (.start m))))
   'format-message  format
   ;; --- list ops ---
   'identity  identity
   'memq      (search-list identical?)
   'memql     (search-list =)
   'rassoc    (fn [key alist] (some (fn [p] (when (and (seq? p) (= key (second p))) p)) alist))
   'rassq     (fn [key alist] (some (fn [p] (when (and (seq? p) (identical? key (second p))) p)) alist))
   'delq      (remove-by identical?)
   'delete    (remove-by =)
   'remq      (remove-by identical?)
   'last  (fn [l & [n]] (seq (drop (max 0 (- (count l) (or n 1))) l)))
   'butlast (fn [l & [n]] (seq (drop-last (or n 1) l)))
   'take      (fn [n l] (seq (clojure.core/take n l)))
   'elt       (fn [s n] (clojure.core/nth s n))
   'aref      (fn [s n] (cond (string? s) (int (.charAt ^String s (int n)))
                               (vector? s) (get s n)
                               :else (clojure.core/nth s n)))
   'vconcat   (fn [& seqs] (vec (apply concat seqs)))
   'make-list   (fn [n init] (apply list (repeat n init)))
   'make-vector (fn [n init] (vec (repeat n init)))
   'vector    vec
   'length<     (fn [seq n] (< (count seq) n))
   'length>     (fn [seq n] (> (count seq) n))
   'length=     (fn [seq n] (= (count seq) n))
   'sort       el-sort
   'mapc       el-mapc
   'mapcan     el-mapcan
   'mapconcat  el-mapconcat
   'plist-member (fn [plist prop]
                             (loop [p (seq plist)]
                               (when (and p (next p))
                                 (if (= (first p) prop) p (recur (nnext p))))))
   ;; --- symbol operations ---
   'set                  el-var!
   'fset                 (fn [sym def] (swap! *fns* assoc sym def) def)
   'symbol-function (fn [sym] (or (get @*fns* sym) (get builtins sym)))
   'symbol-value    (fn [sym] (let [v (var-lookup sym)]
                                          (when (= v ::not-found) (err (str "Void variable: " sym))) v))
   'symbol-plist    (fn [sym] (let [pl (get @*plists* sym)]
                                          (when pl (apply list (mapcat identity pl)))))
   'setplist              (fn [sym plist] (swap! *plists* assoc sym (apply hash-map plist)) plist)
   'indirect-function (fn [sym] (if (symbol? sym) (resolve-fn sym) sym))
   'makunbound            (fn [sym] (swap! *vars* dissoc sym) sym)
   'fmakunbound           (fn [sym] (swap! *fns* dissoc sym) sym)
   'default-value    el-var
   'set-default      el-var!
   'default-boundp   (fn [sym] (contains? @*vars* sym))
   'kill-local-variable (fn [sym] (update-buf! #(update % :locals dissoc sym)) sym)
   'kill-all-local-variables (fn [] (update-buf! #(assoc % :locals {})) nil)
   ;; --- print ---
   'prin1-to-string    el-prin1-to-string
   'prin1      (fn [obj & _] (el-message "%s" (el-prin1-to-string obj)) obj)
   'princ      (fn [obj & _] (el-message "%s" (el-princ-to-string obj)) obj)
   'print      (fn [obj & _] (el-message "\n%s\n" (el-prin1-to-string obj)) obj)
   'terpri     (fn [& _] (el-message "\n") nil)
   'error-message-string (fn [err] (if (seq? err) (str (second err)) (str err)))
   ;; --- buffer position ---
   'bobp         el-bobp
   'eobp         el-eobp
   'bolp         el-bolp
   'eolp         el-eolp
   'char-after     el-char-after
   'char-before    el-char-before
   'following-char (fn [] (or (el-char-after) 0))
   'preceding-char (fn [] (or (el-char-before) 0))
   'buffer-size    el-buffer-size
   'buffer-substring               el-buffer-substring
   'buffer-substring-no-properties el-buffer-substring
   'erase-buffer   el-erase-buffer
   'line-beginning-position  el-bol-pos
   'line-end-position        el-eol-pos
   'pos-bol                  el-bol-pos
   'pos-eol                  el-eol-pos
   'forward-line   el-forward-line
   ;; --- buffer management ---
   'current-buffer            el-current-buffer
   'set-buffer                el-set-buffer
   'get-buffer                el-get-buffer
   'get-buffer-create         el-get-buffer-create
   'buffer-list               el-buffer-list
   'kill-buffer               el-kill-buffer
   'buffer-file-name          el-buffer-file-name
   'buffer-modified-p         el-buffer-modified-p
   'buffer-live-p             el-buffer-live-p
   'generate-new-buffer-name  el-generate-new-buffer-name
   'other-buffer              el-other-buffer
   'set-buffer-modified-p     (fn [flag] (update-buf! #(assoc % :modified (boolean flag))) flag)
   'rename-buffer             (fn [name & _] (update-buf! #(assoc % :name (str name))) name)
   'buffer-local-variables    (fn [] (apply list (map (fn [[k v]] (cons k v)) (buf-locals))))
   ;; --- file I/O ---
   'file-exists-p       (file-check #(.exists ^java.io.File %))
   'file-directory-p    (file-check #(.isDirectory ^java.io.File %))
   'file-readable-p     (file-check #(.canRead ^java.io.File %))
   'file-writable-p     (file-check #(.canWrite ^java.io.File %))
   'file-executable-p   (file-check #(.canExecute ^java.io.File %))
   'file-regular-p      (file-check #(.isFile ^java.io.File %))
   'file-symlink-p      (file-check #(java.nio.file.Files/isSymbolicLink (.toPath ^java.io.File %)))
   'file-name-directory     (file-check #(when-let [par (.getParent ^java.io.File %)] (str par "/")))
   'file-name-nondirectory  (file-check #(.getName ^java.io.File %))
   'file-name-absolute-p    (file-check #(.isAbsolute ^java.io.File %))
   'directory-file-name     (fn [p] (let [s (str p)] (if (.endsWith s "/") (subs s 0 (dec (count s))) s)))
   'directory-name-p        (fn [p] (.endsWith ^String (str p) "/"))
   'file-name-as-directory  (fn [p] (let [s (str p)] (if (.endsWith s "/") s (str s "/"))))
   'file-newer-than-file-p  (fn [a b] (> (.lastModified (->file a)) (.lastModified (->file b))))
   'expand-file-name        el-expand-file-name
   'file-truename           (fn [p] (.getCanonicalPath (->file p)))
   'delete-file             (fn [p & _] (.delete (->file p)) nil)
   'rename-file             (fn [a b & _] (.renameTo (->file a) (->file b)))
   'copy-file               (fn [a b & _] (java.nio.file.Files/copy
                                        (.toPath (->file a)) (.toPath (->file b))
                                        (into-array java.nio.file.CopyOption
                                          [java.nio.file.StandardCopyOption/REPLACE_EXISTING])))
   'make-directory           (fn [p & _] (.mkdirs (->file p)))
   'directory-files          (fn [dir & [full match]]
                                         (let [f (java.io.File. (str dir))
                                               files (vec (.list f))
                                               files (if match
                                                       (filterv #(re-find (re-pattern (str match)) %) files)
                                                       files)]
                                           (apply list (if full (mapv #(str dir "/" %) files) files))))
   'insert-file-contents     el-insert-file-contents
   'write-region             el-write-region
   'file-name-extension      (fn [f & _] (let [n (str f) i (.lastIndexOf n ".")]
                                                      (when (pos? i) (subs n (inc i)))))
   'file-name-sans-extension (fn [f] (let [n (str f) i (.lastIndexOf ^String n ".")]
                                                  (if (pos? i) (subs n 0 i) n)))
   'file-name-base          (fn [f] (let [n (.getName (java.io.File. (str f)))
                                                     i (.lastIndexOf ^String n ".")]
                                                 (if (pos? i) (subs n 0 i) n)))
   'locate-user-emacs-file   (fn [name & _]
                                         (str (System/getProperty "user.home") "/.emacs.d/" name))
   'file-name-concat         (fn [& parts] (str/join "/" (map str parts)))
   'substitute-in-file-name  (fn [f] (str f))
   ;; --- environment ---
   'getenv          (fn [var] (System/getenv (str var)))
   'system-name     (fn [] (try (.getHostName (java.net.InetAddress/getLocalHost))
                                       (catch Exception _ "localhost")))
   'emacs-pid       (fn [] (.pid (java.lang.ProcessHandle/current)))
   'user-login-name (sys-prop "user.name")
   'user-full-name  (sys-prop "user.name")
   'user-real-login-name (sys-prop "user.name")
   'user-uid        (constantly 0)
   'user-real-uid   (constantly 0)
   ;; --- time ---
   'float-time      (fn [] (/ (double (System/currentTimeMillis)) 1000.0))
   'current-time-string (fn [& _] (.format (java.text.SimpleDateFormat.
                                               "EEE MMM dd HH:mm:ss yyyy") (java.util.Date.)))
   'current-time    (fn [] (let [ms (System/currentTimeMillis)]
                                       (list (int (/ ms 65536000))
                                             (int (mod (/ ms 1000) 65536))
                                             (int (* (mod ms 1000) 1000)) 0)))
   ;; --- misc ---
   'throw       el-throw
   'sleep-for     (fn [secs & _] (Thread/sleep (long (* secs 1000))) nil)
   'sit-for       (fn [secs & _] (Thread/sleep (long (* secs 1000))) true)
   'garbage-collect (fn [] (System/gc) nil)
   'read-from-string (fn [s] (let [forms (read-all (str s))]
                                         (when (seq forms) (list (first forms) (count (str s))))))
   'intern-soft    (fn [name & _] (let [s (symbol (str name))]
                                              (when (or (contains? @*vars* s)
                                                        (contains? @*fns* s)) s)))
   'mapatoms    (fn [f & _] (doseq [sym (keys @*vars*)] (call-fn f [sym])))
   'recursion-depth (constantly 0)
   ;; --- commonly needed by .el files ---
   'add-to-list (fn [sym elem & _]
                            (let [lst (let [v (var-lookup sym)] (if (= v ::not-found) nil v))]
                              (when-not (some #(= elem %) lst)
                                (swap! *vars* assoc sym (clojure.core/cons elem lst))))
                            elem)
   'y-or-n-p     (fn [prompt] (mini-read (str prompt "(y or n) ")))
   'yes-or-no-p  (fn [prompt] (mini-read (str prompt "(yes or no) ")))
   'key-description (fn [keys & _] (str/join " " (map str keys)))
   'define-prefix-command (fn [sym & _] (el-var! sym (el-make-sparse-keymap)) sym)
   'make-keymap  el-make-sparse-keymap
   'suppress-keymap (fn [km & _] km)
   'string-empty-p (fn [s] (or (nil? s) (= "" (str s))))
   'string-trim    (fn [s & _] (.trim ^String (str s)))
   'string-trim-left (fn [s & _] (.stripLeading ^String (str s)))
   'string-trim-right (fn [s & _] (.stripTrailing ^String (str s)))
   'string-join    (fn [strs & [sep]] (str/join (or sep "") strs))
   'number-sequence (fn [from to & [inc]]
                                (let [inc (or inc 1)]
                                  (apply list (range from (inc to) inc))))
   'current-message (fn [] (:msg @*state*))
   ;; stubs for common init.el calls
   'display-warning       (fn [type msg & _] (el-message "Warning (%s): %s" type msg))
   'warn                  (fn [fmt & args] (apply el-message (str "Warning: " fmt) args))
   'user-error            (fn [fmt & args]
                                      (el-signal 'user-error (list (if (seq args) (apply format fmt args) (str fmt)))))
   'window-width          el-win-cols
   'window-height         el-win-rows
   'frame-width           el-win-cols
   'frame-height          el-win-rows
   'selected-window       (constantly 'main-window)
   'selected-frame        (constantly 'main-frame)
   'window-buffer         (fn [& _] (el-current-buffer))
   'set-window-buffer     (fn [_ buf & _] (el-set-buffer (str buf)))
   'display-buffer        (fn [buf & _] buf)
   'switch-to-buffer      el-switch-to
   'pop-to-buffer         el-switch-to
   'with-selected-window  (fn [_ & body] (eval-body body))
   'make-frame            (constantly 'main-frame)
   'framep                (fn [_] true)
   'windowp               (fn [x] (= x 'main-window))}))

;; --- Eval ---

(def ^:private let*-sym 'let*)

(defn eval
  "Evaluate an elisp form. Requires dynamic vars to be bound."
  [form]
  (cond
    ;; self-evaluating
    (nil? form)     nil
    (number? form)  form
    (string? form)  form
    (char? form)    form
    (vector? form)  (mapv eval form)
    (boolean? form) form

    ;; symbol lookup
    (symbol? form)
    (cond
      (= form 't) true
      (.startsWith (name form) ":") form  ;; keyword symbols are self-quoting
      :else (if-let [cell (get *env* form)]
              @cell
              (let [v (var-lookup form)]
                (if (= v ::not-found)
                  (err (str "Void variable: " form))
                  v))))

    ;; list (special form or function call)
    (seq? form)
    (let [[head & args] form]
      (case head
        quote           (first args)
        backquote       (expand-backquote (first args))
        if              (eval-if args)
        cond            (eval-cond args)
        progn           (eval-body args)
        setq            (eval-setq args)
        let             (eval-let args)
        defun           (eval-def* *fns* args)
        defmacro        (eval-def* *macros* args)
        defvar          (eval-defvar args)
        defcustom       (eval-defvar args)
        lambda          {:args (vec (first args)) :parsed (parse-arglist (first args)) :body (rest args) :env *env*}
        while           (eval-while args)
        and             (eval-and args)
        or              (eval-or args)
        save-excursion  (eval-save-excursion args)
        condition-case  (eval-condition-case args)
        unwind-protect  (eval-unwind-protect args)
        catch           (eval-catch args)
        defconst        (eval-defconst args)
        function        (let [arg (first args)]
                          (if (and (seq? arg) (= (first arg) 'lambda))
                            (eval arg)
                            (resolve-fn arg)))
        save-current-buffer (let [buf (:buf @*state*)]
                              (try (eval-body args)
                                   (finally (swap! *state* assoc :buf buf))))
        (save-restriction eval-when-compile eval-and-compile) (eval-body args)
        declare-function nil
        defvaralias (let [[new old] args
                          v (var-lookup old)]
                      (when-not (= v ::not-found)
                        (swap! *vars* assoc new v))
                      new)
        ;; let* (can't appear in case — Clojure special form) + function call
        (cond
          (= head let*-sym) (eval-let* args)
          :else (if-let [macro (get @*macros* head)]
                  (eval (apply-user-fn macro (vec args)))
                  (let [evaled-args (mapv eval args)]
                    (call-fn (resolve-fn head) evaled-args))))))

    :else (err (str "Cannot eval: " (pr-str form)))))

;; ============================================================
;; PUBLIC API
;; ============================================================

(def ^:private env-bindings
  [[#'*vars*      :el-vars      {}]
   [#'*fns*       :el-fns       {}]
   [#'*macros*    :el-macros    {}]
   [#'*docs*      :el-docs      {}]
   [#'*features*  :el-features  #{}]
   [#'*load-path* :el-load-path [(str (System/getProperty "user.home") "/.xmas/lisp/")]]
   [#'*plists*    :el-plists    {}]
   [#'*loading*   :el-loading   #{}]
   [#'*buffer-local-syms* :el-buffer-local-syms #{}]])

(defn- capture-env
  "Snapshot current dynamic var bindings for later replay."
  []
  (into {} (map (fn [[v]] [v @v])) env-bindings))

(defn- bind-all
  "Bind all dynamic vars from editor state, creating defaults for missing ones."
  [editor-state]
  (into {#'*state* editor-state #'*env* {}}
        (map (fn [[var-ref k default]]
               [var-ref (or (get @editor-state k) (atom default))]))
        env-bindings))

(defn- resolve-keymap-bindings
  "Flatten keymap bindings into {key -> handler} for ed.clj.
   Merges: global-keymap -> local-keymap -> explicit :xmas/bindings."
  []
  (let [wrap (fn [sym-or-fn]
               (cond
                 (symbol? sym-or-fn) (when-let [f (get @*fns* sym-or-fn)]
                                       (make-key-handler f))
                 (map? sym-or-fn)    (make-key-handler sym-or-fn)
                 (fn? sym-or-fn)     sym-or-fn))
        collect (fn [km]
                  (when (keymap? km)
                    (into {} (keep (fn [[k v]] (when-let [h (wrap v)] [k h])))
                          (:bindings km))))
        global-km (el-var :xmas/global-keymap)
        local-km  (el-current-local-map)]
    (merge (collect global-km)
           (collect local-km)
           (el-var :xmas/bindings))))

(defn- persist-all!
  "Save all dynamic var atoms back into editor state."
  [editor-state]
  (swap! editor-state merge
    (into {} (map (fn [[var-ref k _]] [k (deref var-ref)])) env-bindings))
  (let [binds (resolve-keymap-bindings)]
    (when (seq binds)
      (swap! editor-state assoc :el-bindings binds))))

(defn- eval-with-env
  "Bind all Elisp dynamic vars, run f, persist state back."
  [editor-state f]
  (with-bindings (bind-all editor-state)
    (let [result (f)]
      (persist-all! editor-state)
      result)))

(defn eval-string
  "Read and evaluate all forms in an elisp string.
   editor-state is an atom. Returns the last evaluation result."
  [^String s editor-state]
  (let [forms (read-all s)]
    (eval-with-env editor-state #(eval-body forms))))

(defn eval-1
  "Read and evaluate a single elisp expression. Returns the result."
  [^String s editor-state]
  (when-let [form (first (read-all s))]
    (eval-with-env editor-state #(eval form))))

(defn run-hooks!
  "Run named hooks in the context of editor-state (an atom)."
  [editor-state & hook-names]
  (eval-with-env editor-state
    #(apply el-run-hooks hook-names)))

(defn fire-timers!
  "Check and fire expired timers. Call from command loop."
  [editor-state]
  (let [now (System/currentTimeMillis)
        expired (seq (filter #(<= (:next-fire %) now) @el-timers))]
    (when expired
      (eval-with-env editor-state
        #(doseq [t expired]
           (try (call-fn (:fn-sym t) (:args t)) (catch Exception _))))
      (swap! el-timers
        (fn [ts] (into [] (keep #(if (<= (:next-fire %) now)
                                   (when (:repeat-ms %) (assoc % :next-fire (+ now (:repeat-ms %))))
                                   %)) ts))))))
