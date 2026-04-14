(ns xmas.el
  (:refer-clojure :exclude [eval read])
  (:require [xmas.buf :as buf]
            [xmas.gap :as gap]
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

(defn- ws? [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

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

(defn- read-list [^PushbackReader rdr]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch) (err "Unterminated list")
        (= ch \)) (do (read-ch rdr) (apply list acc))
        :else     (recur (conj acc (read rdr)))))))

(defn- read-vector [^PushbackReader rdr]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch) (err "Unterminated vector")
        (= ch \]) (do (read-ch rdr) acc)
        :else     (recur (conj acc (read rdr)))))))

(defn- read-quote [^PushbackReader rdr]
  (let [form (read rdr)]
    (when (= form ::eof) (err "Unexpected EOF after quote"))
    (list 'quote form)))

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
      (= ch \() (read-list rdr)
      (= ch \)) (err "Unexpected ')'")
      (= ch \[) (read-vector rdr)
      (= ch \]) (err "Unexpected ']'")
      (= ch \') (read-quote rdr)
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
  (loop [cs clauses]
    (when (seq cs)
      (let [[test & body] (first cs)]
        (if (or (= test 't) (eval test))
          (eval-body body)
          (recur (rest cs)))))))

(defn- eval-setq [args]
  (loop [pairs (partition 2 args) result nil]
    (if-not (seq pairs)
      result
      (let [[sym val] (first pairs)
            v (eval val)]
        (swap! *vars* assoc sym v)
        (recur (rest pairs) v)))))

(defn- eval-let [args]
  (let [[bindings & body] args
        ;; elisp let: ((sym val) (sym val) ...) — each binding is a list
        saved (into {} (map (fn [b] (let [sym (first b)]
                                      [sym (get @*vars* sym ::unbound)]))
                            bindings))]
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

(defn- eval-defun [[name arglist & body]]
  (swap! *fns* assoc name {:args (vec arglist) :body body})
  name)

(defn- eval-while [[test & body]]
  (loop []
    (when (eval test)
      (eval-body body)
      (recur))))

(defn- eval-and [args]
  (loop [forms args result true]
    (if-not (seq forms) result
      (let [v (eval (first forms))]
        (if-not v nil (recur (rest forms) v))))))

(defn- eval-or [args]
  (loop [forms args]
    (when (seq forms)
      (let [v (eval (first forms))]
        (if v v (recur (rest forms)))))))

;; --- Built-in functions ---

(defn- apply-user-fn [{:keys [args body]} call-args]
  (when (not= (count args) (count call-args))
    (err (str "Wrong number of arguments: expected " (count args) ", got " (count call-args))))
  (let [saved (into {} (map (fn [sym] [sym (get @*vars* sym ::unbound)]) args))]
    (doseq [[sym val] (map vector args call-args)]
      (swap! *vars* assoc sym val))
    (try
      (eval-body body)
      (finally
        (doseq [[sym old] saved]
          (if (= old ::unbound)
            (swap! *vars* dissoc sym)
            (swap! *vars* assoc sym old)))))))

;; --- Buffer bridge ---

(defn- el-point [] (:point (get (:bufs @*state*) (:buf @*state*))))
(defn- el-point-min [] 0)
(defn- el-point-max [] (count (:text (get (:bufs @*state*) (:buf @*state*)))))

(defn- el-goto-char [n]
  (swap! *state* (fn [s]
    (update-in s [:bufs (:buf s)]
      #(assoc % :point (max 0 (min n (count (:text %))))))))
  n)

(defn- el-forward-char [& [n]]
  (let [n (or n 1)]
    (dotimes [_ n]
      (swap! *state* (fn [s]
        (update-in s [:bufs (:buf s)]
          #(buf/set-point % (fn [t p] (text/next-pos t p)))))))))

(defn- el-backward-char [& [n]]
  (let [n (or n 1)]
    (dotimes [_ n]
      (swap! *state* (fn [s]
        (update-in s [:bufs (:buf s)]
          #(buf/set-point % (fn [t p] (text/prev-pos t p)))))))))

(defn- el-beginning-of-line []
  (swap! *state* (fn [s]
    (update-in s [:bufs (:buf s)]
      #(buf/set-point % (fn [t p] (gap/nth-line-start t (gap/line-of t p))))))))

(defn- el-end-of-line []
  (swap! *state* (fn [s]
    (update-in s [:bufs (:buf s)]
      #(buf/set-point % (fn [t p] (gap/nth-line-end t (gap/line-of t p))))))))

(defn- el-insert [& strings]
  (doseq [s strings]
    (let [text (str s)]
      (swap! *state* (fn [st]
        (let [b (get (:bufs st) (:buf st))
              p (:point b)]
          (update-in st [:bufs (:buf st)] #(buf/edit % p p text))))))))

(defn- el-delete-region [from to]
  (let [lo (min from to) hi (max from to)]
    (swap! *state* (fn [s]
      (update-in s [:bufs (:buf s)] #(buf/edit % lo hi ""))))))

(defn- el-buffer-string []
  (str (:text (get (:bufs @*state*) (:buf @*state*)))))

(defn- el-buffer-name []
  (:name (get (:bufs @*state*) (:buf @*state*))))

(defn- el-search-forward [pattern]
  (let [b (get (:bufs @*state*) (:buf @*state*))
        found (text/search-forward (:text b) pattern (inc (:point b)))]
    (when found (el-goto-char (+ found (count pattern))))
    found))

(defn- el-search-backward [pattern]
  (let [b (get (:bufs @*state*) (:buf @*state*))
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
                  (binding [*state* (atom s) *vars* *vars* *fns* *fns*]
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
   'listp   seq?,     'symbolp symbol?
   'equal   =,        'not     not
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
   'buffer-string     el-buffer-string
   'buffer-name       el-buffer-name
   'search-forward    el-search-forward
   'search-backward   el-search-backward
   'message           el-message
   'global-set-key    el-global-set-key})

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

    ;; symbol lookup
    (symbol? form)
    (cond
      (= form 't) true
      (contains? @*vars* form) (get @*vars* form)
      :else (err (str "Void variable: " form)))

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

(defn eval-string
  "Read and evaluate all forms in an elisp string.
   editor-state is an atom. Returns the last evaluation result."
  [^String s editor-state]
  (let [forms (read-all s)]
    (binding [*vars* (or (:el-vars @editor-state) (atom {}))
              *fns* (or (:el-fns @editor-state) (atom {}))
              *state* editor-state]
      (let [result (reduce (fn [_ form] (eval form)) nil forms)]
        ;; Persist the elisp environment in the editor state
        (swap! editor-state assoc :el-vars *vars* :el-fns *fns*)
        ;; Merge any user-defined keybindings
        (when-let [user-binds (get @*vars* :xmas/bindings)]
          (swap! editor-state assoc :el-bindings user-binds))
        result))))

(defn eval-1
  "Read and evaluate a single elisp expression. Returns the result."
  [^String s editor-state]
  (let [forms (read-all s)]
    (when (seq forms)
      (binding [*vars* (or (:el-vars @editor-state) (atom {}))
                *fns* (or (:el-fns @editor-state) (atom {}))
                *state* editor-state]
        (let [result (eval (first forms))]
          (swap! editor-state assoc :el-vars *vars* :el-fns *fns*)
          (when-let [user-binds (get @*vars* :xmas/bindings)]
            (swap! editor-state assoc :el-bindings user-binds))
          result)))))
