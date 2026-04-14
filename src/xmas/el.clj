(ns xmas.el
  (:refer-clojure :exclude [eval read])
  (:require [xmas.buf :as buf]
            [xmas.text :as text])
  (:import [java.io PushbackReader StringReader]))

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
  (throw (ex-info msg {:detail msg})))

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
          (= ch \") (.toString sb)
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
  (cond
    (= tok "nil") nil
    :else
    (or (try (Long/parseLong tok) (catch NumberFormatException _ nil))
        (try (Double/parseDouble tok) (catch NumberFormatException _ nil))
        (symbol tok))))

(defn- read-token [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (delimiter? ch) (do (when ch (unread-ch rdr ch))
                              (let [tok (.toString sb)]
                                (when (empty? tok) (err "Unexpected delimiter"))
                                (parse-token tok)))
          :else           (do (.append sb ch) (recur)))))))

(defn read
  "Read one elisp form from a PushbackReader. Returns ::eof on end of input."
  [^PushbackReader rdr]
  (skip-ws rdr)
  (let [ch (read-ch rdr)]
    (cond
      (nil? ch) ::eof
      (= ch \") (read-string-literal rdr)
      (= ch \() (read-delimited rdr \) "list" #(apply list %))
      (= ch \)) (err "Unexpected ')'")
      (= ch \[) (read-delimited rdr \] "vector" identity)
      (= ch \]) (err "Unexpected ']'")
      (= ch \') (read-wrapping rdr 'quote "quote")
      (= ch \`) (read-wrapping rdr 'backquote "backquote")
      (= ch \,) (read-unquote rdr)
      (= ch \?) (read-char-literal rdr)
      :else     (do (unread-ch rdr ch) (read-token rdr)))))

(defn read-all
  "Read all elisp forms from a string. Returns a vector."
  [^String s]
  (let [rdr (PushbackReader. (StringReader. s))]
    (loop [forms []]
      (let [form (read rdr)]
        (if (= form ::eof) forms (recur (conj forms form)))))))

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

(declare eval apply-user-fn expand-backquote)

;; --- Buffer helpers (needed by special forms) ---

(defn- el-cur [] (get (:bufs @*state*) (:buf @*state*)))

(defn- update-buf!
  "Apply f to the current buffer in *state*."
  [f]
  (swap! *state* (fn [s] (update-in s [:bufs (:buf s)] f))))

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
          (= a '&rest)     {:required required :optional optional :rest (second args)}
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

(defn- eval-body
  "Eval a sequence of forms (progn semantics). Returns last value."
  [forms]
  (reduce (fn [_ f] (eval f)) nil forms))

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
              (swap! *vars* assoc sym v)
              v))
          nil (partition 2 args)))

(defn- eval-let [args]
  (let [[bindings & body] args
        ;; Evaluate all init-forms BEFORE binding (parallel let semantics)
        evaled (mapv (fn [b] [(first b) (eval (second b))]) bindings)
        saved (into {} (map (fn [[sym _]] [sym (get @*vars* sym ::unbound)]) evaled))]
    (doseq [[sym val] evaled]
      (swap! *vars* assoc sym val))
    (try
      (eval-body body)
      (finally
        (doseq [[sym old] saved]
          (if (= old ::unbound)
            (swap! *vars* dissoc sym)
            (swap! *vars* assoc sym old)))))))

(defn- eval-let* [args]
  (let [[bindings & body] args
        saved (into {} (map (fn [b] (let [sym (first b)]
                                      [sym (get @*vars* sym ::unbound)]))
                            bindings))]
    ;; Bind sequentially (each binding sees previous ones)
    (doseq [b bindings]
      (let [sym (first b) val (second b)]
        (swap! *vars* assoc sym (eval val))))
    (try
      (eval-body body)
      (finally
        (doseq [[sym old] saved]
          (if (= old ::unbound)
            (swap! *vars* dissoc sym)
            (swap! *vars* assoc sym old)))))))

(defn- eval-def* [store [name arglist & body]]
  (let [parsed (parse-arglist arglist)]
    (swap! store assoc name {:args (vec arglist) :parsed parsed :body body})
    name))

(defn- eval-defvar [[name & rest-args]]
  (let [has-value (seq rest-args)
        value (when has-value (first rest-args))
        docstring (when (and (> (count rest-args) 1) (string? (second rest-args)))
                    (second rest-args))]
    (when (and has-value (not (contains? @*vars* name)))
      (swap! *vars* assoc name (eval value)))
    (when docstring
      (swap! *docs* assoc name docstring))
    name))

(defn- eval-while [[test & body]]
  (loop []
    (when (eval test)
      (eval-body body)
      (recur))))

(defn- eval-and [args]
  (reduce (fn [_ form] (let [v (eval form)] (if v v (reduced nil)))) true args))

(defn- eval-or [args]
  (reduce (fn [_ form] (let [v (eval form)] (if v (reduced v) nil))) nil args))

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

(defn- apply-user-fn [{:keys [args parsed body]} call-args]
  (let [parsed (or parsed {:required (vec args) :optional [] :rest nil})
        bindings (bind-args parsed call-args "function")
        all-syms (keys bindings)
        saved (into {} (map (fn [sym] [sym (get @*vars* sym ::unbound)]) all-syms))]
    (doseq [[sym val] bindings]
      (swap! *vars* assoc sym val))
    (try
      (eval-body body)
      (finally
        (doseq [[sym old] saved]
          (if (= old ::unbound)
            (swap! *vars* dissoc sym)
            (swap! *vars* assoc sym old)))))))

;; --- Buffer bridge ---

(defn- el-point [] (:point (el-cur)))
(defn- el-point-min [] 0)
(defn- el-point-max [] (count (:text (el-cur))))

(defn- el-goto-char [n]
  (update-buf! #(assoc % :point (max 0 (min n (count (:text %))))))
  n)

(defn- el-forward-char [& [n]]
  (dotimes [_ (or n 1)]
    (update-buf! #(buf/set-point % text/next-pos))))

(defn- el-backward-char [& [n]]
  (dotimes [_ (or n 1)]
    (update-buf! #(buf/set-point % text/prev-pos))))

(defn- el-beginning-of-line []
  (update-buf! #(buf/set-point % text/line-start)))

(defn- el-end-of-line []
  (update-buf! #(buf/set-point % text/line-end)))

(defn- el-insert [& strings]
  (doseq [s strings]
    (let [text (str s)]
      (update-buf! (fn [b] (buf/edit b (:point b) (:point b) text))))))

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

(defn- el-search-forward [pattern]
  (let [b (el-cur)
        found (text/search-forward (:text b) pattern (inc (:point b)))]
    (when found (el-goto-char (+ found (count pattern))))
    found))

(defn- el-search-backward [pattern]
  (let [b (el-cur)
        found (text/search-backward (:text b) pattern (:point b))]
    (when found (el-goto-char found))
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
            (case mod
              \C (do (read-ch rdr) ;; skip the '-'
                     (let [c (read-ch rdr)]
                       (recur (conj keys [:ctrl c]))))
              \M (do (read-ch rdr)
                     (let [c (read-ch rdr)]
                       (recur (conj keys [:meta c]))))
              ;; literal backslash followed by char
              (recur (conj keys (char mod)))))
          :else (recur (conj keys ch)))))))

;; --- Keybinding bridge ---

(defn- el-global-set-key [key-str func-sym]
  (let [keys (parse-key-string key-str)
        elisp-fn (or (get @*fns* func-sym)
                     (err (str "Void function: " func-sym)))
        handler (fn [s]
                  (binding [*state* (atom s) *vars* *vars* *fns* *fns*
                            *macros* *macros* *docs* *docs*
                            *features* *features* *load-path* *load-path*
                            *plists* *plists* *loading* *loading*]
                    (apply-user-fn elisp-fn [])
                    @*state*))]
    (if (= 1 (count keys))
      (swap! *vars* assoc :xmas/bindings
             (assoc (get @*vars* :xmas/bindings {}) (first keys) handler))
      (let [prefix (first keys)
            suffix (second keys)]
        (swap! *vars* assoc :xmas/bindings
               (update (get @*vars* :xmas/bindings {}) prefix
                       #(assoc (or % {}) suffix handler)))))))

;; --- Load system ---

(defn- el-load-file [filename]
  (let [content (slurp filename)
        forms (read-all content)]
    (eval-body forms)
    true))

(defn- el-load [name-or-path]
  (let [name-str (if (symbol? name-or-path) (clojure.core/name name-or-path) (str name-or-path))
        load-path (or (seq (get @*vars* (symbol "load-path")))
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
      (el-load (clojure.core/name feature))
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
  (loop [pl (seq plist) acc [] found false]
    (if-not pl
      (apply list (if found acc (concat acc [prop val])))
      (if (and (not found) (= (first pl) prop) (next pl))
        (recur (nnext pl) (conj acc prop val) true)
        (recur (next pl) (conj acc (first pl)) found)))))

;; --- Macro expansion ---

(defn- el-macroexpand [form]
  (if (and (seq? form) (symbol? (first form)) (contains? @*macros* (first form)))
    (let [macro (get @*macros* (first form))]
      (apply-user-fn macro (rest form)))
    form))

;; --- Resolve and call functions ---

(declare builtins)

(defn- resolve-fn [sym]
  (or (get @*fns* sym)
      (get builtins sym)
      (err (str "Void function: " sym))))

(defn- call-fn
  "Resolve f (symbol, lambda map, or builtin) and call with args."
  [f args]
  (let [actual-f (if (symbol? f) (resolve-fn f) f)]
    (if (map? actual-f)
      (apply-user-fn actual-f (vec args))
      (clojure.core/apply actual-f args))))

;; --- Built-in function table ---

(def ^:private builtins
  {;; arithmetic
   '+       +,       '-       -,       '*       *
   '/       /,       'mod     mod
   '<       <,       '>       >,       '=       ==
   '<=      <=,      '>=      >=
   (symbol "1+") inc
   (symbol "1-") dec
   ;; list
   'cons    cons,    'car     first,   'cdr     next
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
   'listp    (fn [x] (or (nil? x) (seq? x)))
   'consp    seq?
   'symbolp  symbol?
   'equal    =,        'not      not
   'boundp   (fn [sym] (contains? @*vars* sym))
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
               (err (if (seq args) (clojure.core/apply format fmt args) (str fmt))))
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
   'search-forward    el-search-forward
   'search-backward   el-search-backward
   'message           el-message
   'global-set-key    el-global-set-key
   ;; load system
   'provide           el-provide
   'require           el-require
   'load              el-load
   (symbol "load-file") el-load-file
   'featurep          el-featurep
   ;; property lists
   'put               el-put
   'get               el-get
   'plist-get         el-plist-get
   'plist-put         el-plist-put
   ;; macros
   'macroexpand       el-macroexpand})

;; --- Eval ---

(def ^:private let*-sym (symbol "let*"))

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
    (true? form)    true
    (false? form)   false

    ;; symbol lookup
    (symbol? form)
    (cond
      (= form 't) true
      (contains? @*vars* form) (get @*vars* form)
      :else (err (str "Void variable: " form)))

    ;; list (special form or function call)
    (seq? form)
    (let [[head & args] form]
      (cond
        (= head let*-sym) (eval-let* args)

        :else
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
          lambda          {:args (vec (first args)) :parsed (parse-arglist (first args)) :body (rest args)}
          while           (eval-while args)
          and             (eval-and args)
          or              (eval-or args)
          save-excursion  (eval-save-excursion args)
          ;; function call or macro expansion
          (if-let [macro (get @*macros* head)]
            (eval (apply-user-fn macro (vec args)))
            (let [evaled-args (mapv eval args)
                  f (or (get @*fns* head)
                        (get builtins head)
                        (err (str "Void function: " head)))]
              (if (map? f)
                (apply-user-fn f evaled-args)
                (apply f evaled-args)))))))

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
   [#'*loading*   :el-loading   #{}]])

(defn- bind-all
  "Bind all dynamic vars from editor state, creating defaults for missing ones."
  [editor-state]
  (into {#'*state* editor-state}
        (map (fn [[var-ref k default]]
               [var-ref (or (get @editor-state k) (atom default))]))
        env-bindings))

(defn- persist-all!
  "Save all dynamic var atoms back into editor state."
  [editor-state]
  (swap! editor-state assoc
    :el-vars *vars* :el-fns *fns* :el-macros *macros*
    :el-docs *docs* :el-features *features*
    :el-load-path *load-path* :el-plists *plists* :el-loading *loading*)
  (when-let [user-binds (get @*vars* :xmas/bindings)]
    (swap! editor-state assoc :el-bindings user-binds)))

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
  (let [forms (read-all s)]
    (when (seq forms)
      (eval-with-env editor-state #(eval (first forms))))))
