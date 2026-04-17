(ns xmas.el
  (:refer-clojure :exclude [eval read])
  (:require [xmas.cmd :as cmd]
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

(defn- err [msg] (throw (ex-info msg {})))

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

(def ^:private delimiters #{\( \) \[ \] \" \' \; })

(defn- delimiter? [ch]
  (or (nil? ch) (ws? ch) (contains? delimiters ch)))

(def ^:private string-escapes {\\ \\, \" \", \n \newline, \t \tab})
(def ^:private char-escapes   {\\ \\, \n \newline, \t \tab, \s \space})

(defn- read-string-literal [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (nil? ch) (err "Unterminated string literal")
          (= ch \") (.toString sb)
          (= ch \\) (let [esc (read-ch rdr)]
                      (cond
                        (nil? esc) (err "Unterminated string escape")
                        (string-escapes esc) (do (.append sb ^Character (string-escapes esc)) (recur))
                        :else (err (str "Unknown string escape: \\" esc))))
          :else     (do (.append sb ch) (recur)))))))

(declare read)

(defn- read-seq
  "Read forms until close, apply finalize. kind is used only in error messages."
  [^PushbackReader rdr close kind finalize]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch)   (err (str "Unterminated " kind))
        (= ch close) (do (read-ch rdr) (finalize acc))
        :else       (recur (conj acc (read rdr)))))))

(defn- read-quote [^PushbackReader rdr]
  (let [form (read rdr)]
    (when (= form ::eof) (err "Unexpected EOF after quote"))
    (list 'quote form)))

(defn- read-char-literal [^PushbackReader rdr]
  (let [ch (read-ch rdr)]
    (cond
      (nil? ch)   (err "Unexpected EOF after ?")
      (not= ch \\) ch
      :else (let [esc (read-ch rdr)]
              (cond
                (nil? esc)        (err "Unexpected EOF in character literal")
                (char-escapes esc) (char-escapes esc)
                :else             (err (str "Unknown character escape: \\" esc)))))))

(defn- parse-token [^String tok]
  (cond
    (= tok "nil") nil
    :else
    (or (try (Long/parseLong tok) (catch NumberFormatException _ nil))
        (try (Double/parseDouble tok) (catch NumberFormatException _ nil))
        (symbol tok))))

(defn- read-token
  "Consume a run of non-delimiter chars. Called only after `read` has unread a
   non-delimiter, so the accumulated token is guaranteed non-empty."
  [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (if (delimiter? ch)
          (do (when ch (unread-ch rdr ch))
              (parse-token (.toString sb)))
          (do (.append sb ch) (recur)))))))

(defn read
  "Read one elisp form from a PushbackReader. Returns ::eof on end of input."
  [^PushbackReader rdr]
  (skip-ws rdr)
  (let [ch (read-ch rdr)]
    (if (nil? ch) ::eof
      (case ch
        \" (read-string-literal rdr)
        \( (read-seq rdr \) "list" #(apply list %))
        \) (err "Unexpected ')'")
        \[ (read-seq rdr \] "vector" identity)
        \] (err "Unexpected ']'")
        \' (read-quote rdr)
        \? (read-char-literal rdr)
        (do (unread-ch rdr ch) (read-token rdr))))))

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

(def ^:dynamic *vars* nil)    ;; atom {symbol → value}
(def ^:dynamic *fns* nil)     ;; atom {symbol → {:args [...] :body [...]}}
(def ^:dynamic *state* nil)   ;; atom of editor state

(declare eval apply-user-fn)

;; --- Special forms ---

(defn- eval-body
  "Eval a sequence of forms (progn semantics). Returns last value."
  [forms]
  (reduce (fn [_ f] (eval f)) nil forms))

(defn- eval-if [[test then & else]]
  (if (eval test) (eval then) (eval-body else)))

(defn- eval-cond [clauses]
  (reduce (fn [_ [test & body]]
            (when (or (= test 't) (eval test))
              (reduced (eval-body body))))
          nil clauses))

(defn- eval-setq [args]
  (reduce (fn [_ [sym init]]
            (let [v (eval init)] (swap! *vars* assoc sym v) v))
          nil (partition 2 args)))

(defn- with-scope
  "Bind syms→vals in *vars* for the duration of thunk, restoring prior state on exit."
  [syms vals thunk]
  (let [outer @*vars*]
    (swap! *vars* merge (zipmap syms vals))
    (try (thunk)
         (finally
           (swap! *vars*
             (fn [m] (reduce (fn [m s]
                               (if (contains? outer s) (assoc m s (outer s)) (dissoc m s)))
                             m syms)))))))

(defn- eval-let [[bindings & body]]
  (let [pairs (map #(if (symbol? %) [% nil] %) bindings)
        ;; elisp `let` binds in parallel: inits see the OUTER env, not each other.
        syms (map first pairs)
        vals (mapv (fn [[_ init]] (eval init)) pairs)]
    (with-scope syms vals #(eval-body body))))

(defn- eval-defun [[name arglist & body]]
  (swap! *fns* assoc name {:args (vec arglist) :body body})
  name)

(defn- eval-while [[test & body]]
  (loop []
    (when (eval test)
      (eval-body body)
      (recur))))

(defn- eval-and [args]
  (reduce (fn [_ f] (or (eval f) (reduced nil))) true args))

(defn- eval-or [args]
  (reduce (fn [_ f] (when-let [v (eval f)] (reduced v))) nil args))

;; --- Built-in functions ---

(defn- apply-user-fn [{:keys [args body]} call-args]
  (when (not= (count args) (count call-args))
    (err (str "Wrong number of arguments: expected " (count args) ", got " (count call-args))))
  (with-scope args call-args #(eval-body body)))

;; --- Buffer bridge ---
;; Each builtin below is a thin adapter: it wraps a pure `xmas.cmd` op by
;; swapping *state*, and returns whatever value elisp expects.

(defn- swap-cmd!
  "Apply a pure state→state cmd (optionally with args) to *state*. Returns nil."
  [cmd & args]
  (swap! *state* #(apply cmd % args))
  nil)

(defn- cur-buf [] (cmd/cur @*state*))

(defn- search-and-move [find-fn offset]
  (fn [pattern]
    (let [b (cur-buf)
          found (find-fn (:text b) pattern (:point b))]
      (when found (swap-cmd! cmd/goto-char (+ found (long (offset pattern)))))
      found)))

(defn- el-message [fmt & args]
  (let [m (if (seq args) (apply format fmt args) (str fmt))]
    (swap! *state* assoc :msg m)
    m))

;; --- Key string parsing ---

(def ^:private key-token-re #"\\C-(.)|\\M-(.)|\\(.)|(.)")

(defn parse-key-string
  "Parse an Emacs key string like \"\\C-x\\C-s\" into a vector of internal key representations."
  [^String s]
  (mapv (fn [[_ c m lit plain]]
          (cond c   [:ctrl (.charAt ^String c 0)]
                m   [:meta (.charAt ^String m 0)]
                lit (.charAt ^String lit 0)
                :else (.charAt ^String plain 0)))
        (re-seq key-token-re s)))

;; --- Keybinding bridge ---

(defn- el-global-set-key [key-str func-sym]
  (let [keys (parse-key-string key-str)
        elisp-fn (or (get @*fns* func-sym)
                     (err (str "Void function: " func-sym)))
        handler (bound-fn* (fn [s]
                             (binding [*state* (atom s)]
                               (apply-user-fn elisp-fn [])
                               @*state*)))]
    (swap! *state* assoc-in (into [:el-bindings] keys) handler)))

;; --- Built-in function table ---

(def ^:private builtins
  {;; arithmetic
   '+       +,       '-       -,       '*       *
   '/       /,       'mod     mod
   '<       <,       '>       >,       '=       ==
   '<=      <=,      '>=      >=
   ;; list
   'cons    cons,    'car     first,   'cdr     rest
   'list    list,    'length  count,   'nth     (fn [n l] (nth l n))
   'null    nil?,    'append  concat
   ;; string
   'concat  str
   'substring (fn
                ([s from] (subs s from))
                ([s from to] (subs s from to)))
   'format  format
   ;; predicates
   'numberp number?,  'stringp string?
   'listp   (fn [x] (or (nil? x) (seq? x))), 'symbolp symbol?
   'equal   =,        'not     not
   ;; buffer — read-only
   'point         #(:point (cur-buf))
   'point-min     (constantly 0)
   'point-max     #(count (:text (cur-buf)))
   'buffer-string #(str (:text (cur-buf)))
   'buffer-name   #(:name (cur-buf))
   ;; buffer — mutating (return nil unless the elisp contract says otherwise)
   'goto-char         (fn [n] (swap-cmd! cmd/goto-char n) n)
   'forward-char      (fn [& [n]] (swap-cmd! cmd/forward-char (or n 1)))
   'backward-char     (fn [& [n]] (swap-cmd! cmd/backward-char (or n 1)))
   'beginning-of-line #(swap-cmd! cmd/beginning-of-line)
   'end-of-line       #(swap-cmd! cmd/end-of-line)
   'insert            (fn [& xs] (doseq [x xs] (swap-cmd! cmd/insert-at-point (str x))))
   'delete-region     #(swap-cmd! cmd/delete-region %1 %2)
   ;; search returns the match position (or nil)
   'search-forward  (search-and-move text/search-forward count)
   'search-backward (search-and-move text/search-backward (constantly 0))
   'message         el-message
   'global-set-key  el-global-set-key})

;; --- Eval ---

(defn eval
  "Evaluate an elisp form. Requires *vars*, *fns*, and *state* to be bound."
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

    ;; symbol lookup (`find` distinguishes "bound to nil" from "unbound")
    (symbol? form)
    (if (= form 't) true
      (if-let [entry (find @*vars* form)]
        (val entry)
        (err (str "Void variable: " form))))

    ;; empty list — elisp `()` is `nil`
    (and (seq? form) (empty? form)) nil

    ;; list (special form or function call)
    (seq? form)
    (let [[head & args] form]
      (case head
        quote   (first args)
        if      (eval-if args)
        cond    (eval-cond args)
        progn   (eval-body args)
        setq    (eval-setq args)
        let     (eval-let args)
        defun   (eval-defun args)
        lambda  {:args (vec (first args)) :body (rest args)}
        while   (eval-while args)
        and     (eval-and args)
        or      (eval-or args)
        ;; function call
        (let [evaled-args (mapv eval args)
              f (or (get @*fns* head)
                    (get builtins head)
                    (err (str "Void function: " head)))]
          (if (map? f)
            (apply-user-fn f evaled-args)
            (apply f evaled-args)))))

    :else (err (str "Cannot eval: " (pr-str form)))))

;; ============================================================
;; PUBLIC API
;; ============================================================

(defn- ensure-env! [editor-state]
  (swap! editor-state
         #(-> % (update :el-vars (fn [x] (or x (atom {}))))
                (update :el-fns  (fn [x] (or x (atom {})))))))

(defn- with-el-env
  "Run f with elisp dynamic scope bound from editor-state's shared env atoms.
   (User keybindings go straight onto :el-bindings via `global-set-key`.)"
  [editor-state f]
  (ensure-env! editor-state)
  (binding [*vars*  (:el-vars @editor-state)
            *fns*   (:el-fns @editor-state)
            *state* editor-state]
    (f)))

(defn eval-string
  "Read and evaluate all forms in an elisp string.
   editor-state is an atom. Returns the last evaluation result."
  [^String s editor-state]
  (with-el-env editor-state
    #(reduce (fn [_ form] (eval form)) nil (read-all s))))

(defn eval-1
  "Read and evaluate a single elisp expression. Returns the result."
  [^String s editor-state]
  (let [forms (read-all s)]
    (when (seq forms)
      (with-el-env editor-state #(eval (first forms))))))
