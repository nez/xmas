(ns xmas.el
  (:refer-clojure :exclude [eval read])
  (:require [xmas.cmd :as cmd]
            [xmas.mode :as mode]
            [xmas.overlay :as ov]
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

(defn- err
  "Raise an elisp `error` signal. Catchable from `condition-case` with
   condition `error` (or `t`). Matches Emacs: built-in errors like
   Void-function / Void-variable / Wrong-number-of-arguments are all
   children of the `error` condition."
  [msg]
  (throw (ex-info msg {:emacs/signal 'error :emacs/data (list msg)})))

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

(def ^:private delimiters #{\( \) \[ \] \" \' \; \` \,})

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

(defn- read-prefix
  "Read the next form, wrap it as (sym form). `label` is shown if EOF hits."
  [^PushbackReader rdr sym ^String label]
  (let [form (read rdr)]
    (when (= form ::eof) (err (str "Unexpected EOF after " label)))
    (list sym form)))

(defn- read-quote [rdr] (read-prefix rdr 'quote "quote"))
(defn- read-quasi [rdr] (read-prefix rdr 'quasiquote "`"))

(defn- read-unquote [^PushbackReader rdr]
  (let [splice? (= \@ (peek-ch rdr))]
    (when splice? (read-ch rdr))
    (read-prefix rdr (if splice? 'unquote-splicing 'unquote) ",")))

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

(def ^:private num-re #"^[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?$")

(defn- parse-token [^String tok]
  (cond
    (= tok "nil") nil
    :else
    ;; Gate number parsing on a strict regex — Double/parseDouble accepts
    ;; "NaN", "Infinity", "+Infinity", etc., which would swallow elisp
    ;; symbols of those names and turn them into floats.
    (or (when (re-matches num-re tok)
          (or (try (Long/parseLong tok) (catch NumberFormatException _ nil))
              (try (Double/parseDouble tok) (catch NumberFormatException _ nil))))
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
        \` (read-quasi rdr)
        \, (read-unquote rdr)
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
(def ^:dynamic *macros* nil)  ;; atom {symbol → {:args [...] :body [...]}}
(def ^:dynamic *state* nil)   ;; atom of editor state
(def ^:dynamic *original* nil);; thunk invoked by (call-original) inside :around advice

(declare eval apply-user-fn)

;; --- Errors / non-local exits ---
;; Represented as Clojure ExceptionInfo values carrying :emacs/signal or :emacs/throw.

(defn- elisp-signal-ex [sym data]
  (ex-info (str "elisp signal " sym) {:emacs/signal sym :emacs/data data}))

(defn- elisp-throw-ex [tag value]
  (ex-info (str "elisp throw " tag) {:emacs/throw tag :emacs/value value}))

;; --- Special forms ---

(defn- eval-body
  "Eval a sequence of forms (progn semantics). Returns last value."
  [forms]
  (reduce (fn [_ f] (eval f)) nil forms))

(defn- eval-if [[test then & else]]
  (if (eval test) (eval then) (eval-body else)))

(defn- eval-cond [clauses]
  ;; `(cond (TEST BODY...))` — if BODY is empty and TEST is truthy, return
  ;; TEST's evaluated value (the elisp idiom `(cond (x) ...)` falls through
  ;; with `x`'s value). Previously this returned nil unconditionally.
  (reduce (fn [_ [test & body]]
            (let [tv (if (= test 't) true (eval test))]
              (when tv
                (reduced (if (seq body) (eval-body body) tv)))))
          nil clauses))

(defn- eval-setq [args]
  (reduce (fn [_ [sym init]]
            (let [v (eval init)] (swap! *vars* assoc sym v) v))
          nil (partition 2 args)))

(defn- restore-vars!
  "Pop `syms` from *vars*, restoring each to its value in `outer` (or
   unbinding it if it wasn't in outer). Shared by with-scope and let*."
  [outer syms]
  (swap! *vars*
         (fn [m] (reduce (fn [m s]
                           (if (contains? outer s) (assoc m s (outer s)) (dissoc m s)))
                         m syms))))

(defn- with-scope
  "Bind syms→vals in *vars* for the duration of thunk, restoring prior state on exit."
  [syms vals thunk]
  (let [outer @*vars*]
    (swap! *vars* merge (zipmap syms vals))
    (try (thunk) (finally (restore-vars! outer syms)))))

(defn- let-pairs
  "Normalize a let bindings form: `(x)` → `[x nil]`, `((x v))` → `[x v]`."
  [bindings]
  (map #(if (symbol? %) [% nil] %) bindings))

(defn- eval-let [[bindings & body]]
  ;; elisp `let` binds in parallel: inits see the OUTER env, not each other.
  (let [pairs (let-pairs bindings)
        syms (map first pairs)
        vals (mapv (fn [[_ init]] (eval init)) pairs)]
    (with-scope syms vals #(eval-body body))))

(defn- eval-let*
  "`let*` binds sequentially — each init sees the prior pairs already bound."
  [[bindings & body]]
  (let [pairs (let-pairs bindings)
        syms  (map first pairs)
        outer @*vars*]
    (try
      (doseq [[sym init] pairs]
        (swap! *vars* assoc sym (eval init)))
      (eval-body body)
      (finally (restore-vars! outer syms)))))

(defn- register-fn!
  "Install a {:args :body} entry into `tbl` for `name`. Shared by defun and
   defmacro — the two differ only in whether arguments are evaluated at the
   call site (see eval's dispatch), not in storage shape."
  [tbl [name arglist & body]]
  (swap! tbl assoc name {:args (vec arglist) :body body})
  name)

(defn- eval-defun    [args] (register-fn! *fns* args))
(defn- eval-defmacro [args] (register-fn! *macros* args))

(declare eval-quasi)

(defn- eval-quasi-list
  "Walk a list at `level`, splicing unquote-splicing forms at level 0."
  [items level]
  (apply list
    (mapcat (fn [item]
              (if (and (seq? item) (= 'unquote-splicing (first item)) (zero? level))
                (let [v (eval (second item))]
                  (if (sequential? v) v (list v)))
                [(eval-quasi item level)]))
            items)))

(defn eval-quasi
  "Expand `` `form `` respecting nested levels. At level 0, `(unquote x)` → `(eval x)`."
  [form level]
  (cond
    (and (seq? form) (= 'quasiquote (first form)))
    (list 'quasiquote (eval-quasi (second form) (inc level)))

    (and (seq? form) (= 'unquote (first form)))
    (if (zero? level)
      (eval (second form))
      (list 'unquote (eval-quasi (second form) (dec level))))

    (and (seq? form) (= 'unquote-splicing (first form)))
    (if (zero? level)
      (err "unquote-splicing outside list")
      (list 'unquote-splicing (eval-quasi (second form) (dec level))))

    (seq? form) (eval-quasi-list form level)
    (vector? form) (vec (eval-quasi-list form level))
    :else form))

(defn- match-condition?
  "Does the raised `sig` satisfy the handler condition? `cond` is either a
   specific symbol, `error` (matches any signal), or `t` (matches anything)."
  [cond sig]
  (or (= cond 't) (= cond 'error) (= cond sig)))

(defn- eval-condition-case
  "(condition-case VAR BODY HANDLERS...) where each HANDLER is (CONDITION BODY...)."
  [[var-sym body & handlers]]
  (try (eval body)
       (catch clojure.lang.ExceptionInfo e
         (let [data (ex-data e)
               sig  (:emacs/signal data)]
           (if-not sig
             (throw e)
             (if-let [[_ & hbody] (some (fn [[cond :as h]]
                                          (when (match-condition? cond sig) h))
                                        handlers)]
               (if (or (nil? var-sym) (= var-sym 'nil))
                 (eval-body hbody)
                 (with-scope [var-sym]
                             [(cons sig (or (:emacs/data data) (list)))]
                             #(eval-body hbody)))
               (throw e)))))))

(defn- eval-unwind-protect
  "(unwind-protect BODY UNWINDFORMS...) — UNWINDFORMS always run on exit."
  [[body & unwinds]]
  (try (eval body)
       (finally (eval-body unwinds))))

(defn- eval-catch
  "(catch TAG BODY...) — captures `(throw TAG VALUE)`."
  [[tag & body]]
  (let [tv (eval tag)]
    (try (eval-body body)
         (catch clojure.lang.ExceptionInfo e
           (let [data (ex-data e)]
             (if (and (contains? data :emacs/throw) (= tv (:emacs/throw data)))
               (:emacs/value data)
               (throw e)))))))

(defn- eval-define-mode
  "define-(derived|minor)-mode — register `name` under `kind`. Parent /
   docstring / body are accepted for compatibility but not used yet."
  [kind [name & _]]
  (swap! *state* mode/register name kind)
  name)

(defn- eval-defcustom
  "(defcustom name default docstring &keys...) — sets the variable, stores doc."
  [[name default docstring & _]]
  (let [v (eval default)]
    (swap! *vars* assoc name v)
    (when (string? docstring)
      (swap! *state* assoc-in [:custom-docs name] docstring))
    name))

(defn- eval-defgroup
  "(defgroup name members docstring ...) — registers a customization group."
  [[name _members docstring & _]]
  (swap! *state* assoc-in [:custom-groups name] (or docstring ""))
  name)

(defn- eval-defvar
  "(defvar name &optional default docstring) — declare a variable; if already
   bound, keep existing value. `(defvar x)` with no default DECLARES without
   binding (matches Emacs — `(boundp 'x)` returns nil)."
  [args]
  (let [[name & more] args
        [default docstring] more]
    (when (and (seq more) (not (contains? @*vars* name)))
      (swap! *vars* assoc name (eval default)))
    (when (string? docstring)
      (swap! *state* assoc-in [:custom-docs name] docstring))
    name))

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

;; Set of targets whose advice is currently active on this thread. A target
;; re-entered from within its own advice must dispatch without re-applying
;; advice, otherwise advice that calls the target recurses unboundedly.
(def ^:dynamic *advising* #{})

(defn- run-advice
  "Invoke each advice function registered under [:advice target kind] with
   the given call-args. Missing or undefined advice is silently skipped."
  [kind target call-args]
  (doseq [sym (get-in @*state* [:advice target kind])]
    (when-let [f (get @*fns* sym)]
      (apply-user-fn f call-args))))

(defn- call-target
  "Invoke `f` with `call-args`. Handles both user-fn maps and Clojure fns,
   so advice can wrap either."
  [f call-args]
  (if (map? f)
    (apply-user-fn f call-args)
    (apply f call-args)))

(defn- apply-with-advice
  "Layer :around advice around the target call (reverse-fold so that the
   first-registered advice is outermost), then run :before and :after.
   `f` may be either a user-fn map or a builtin Clojure fn."
  [target f call-args]
  (binding [*advising* (conj *advising* target)]
    (run-advice :before target call-args)
    (let [around-syms (get-in @*state* [:advice target :around])
          base (fn [] (call-target f call-args))
          invoker (reduce
                    (fn [inner sym]
                      (fn []
                        (if-let [af (get @*fns* sym)]
                          (binding [*original* inner] (apply-user-fn af call-args))
                          (inner))))
                    base
                    (reverse around-syms))
          result (invoker)]
      (run-advice :after target call-args)
      result)))

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

;; --- Error / non-local exit builtins ---

(defn- el-signal [sym data]
  (throw (elisp-signal-ex sym data)))

(defn- el-error [& args]
  (let [msg (if (and (string? (first args)) (seq (rest args)))
              (apply format args) (str (first args)))]
    (throw (elisp-signal-ex 'error (list msg)))))

(defn- el-throw [tag value]
  (throw (elisp-throw-ex tag value)))

;; --- Symbol introspection ---

(defn- el-symbol-function [sym]
  (or (get @*fns* sym)
      (when-let [b (get @(or *macros* (atom {})) sym)]
        {:macro b})))

(defn- el-symbol-value [sym]
  (if-let [e (find @*vars* sym)] (val e)
    (throw (elisp-signal-ex 'void-variable (list sym)))))

(defn- el-boundp [sym] (contains? @*vars* sym))

(declare builtins)

(defn- el-fboundp [sym]
  (boolean (or (contains? @*fns* sym)
               (and *macros* (contains? @*macros* sym))
               (contains? builtins sym))))

(defn- el-fset [sym f]
  (if (map? f)
    (swap! *fns* assoc sym f)
    (err (str "fset: not a function: " (pr-str f))))
  sym)

(defn- el-set [sym value]
  (swap! *vars* assoc sym value)
  value)

(defn- el-makunbound [sym]
  (swap! *vars* dissoc sym)
  sym)

;; --- Plist on state ---

(defn- el-put [sym prop value]
  (swap! *state* assoc-in [:plists sym prop] value)
  value)

(defn- el-get [sym prop]
  (get-in @*state* [:plists sym prop]))

;; --- Key string parsing ---

(def ^:private key-token-re #"\\C-(.)|\\M-(.)|\\(.)|(.)")

(defn parse-key-string
  "Parse an Emacs key string like \"\\C-x\\C-s\" into a vector of internal key representations."
  [^String s]
  (mapv (fn [[_ c m lit plain]]
          (let [ch (.charAt ^String (or c m lit plain) 0)]
            (cond c   [:ctrl ch]
                  m   [:meta ch]
                  :else ch)))
        (re-seq key-token-re s)))

;; --- Keybinding bridge ---

(defn- make-handler
  "Wrap an elisp function symbol into a (state → state) handler. Resolves
   the fn on each invocation so a later `defun` with the same name takes
   effect on already-bound keys (matching Emacs semantics)."
  [func-sym]
  (bound-fn* (fn [s]
               (binding [*state* (atom s)]
                 (let [f (or (get @*fns* func-sym)
                             (err (str "Void function: " func-sym)))]
                   (apply-user-fn f []))
                 @*state*))))

(defn- bind-key
  "Install `handler` at `keymap-path ++ keys` in `state`. If an intermediate
   node along the path is already bound to a handler (not a map), replace
   it with an empty map — binding `C-x C-f` after `C-x` was a plain command
   used to crash with a ClassCastException out of `assoc-in`."
  [state keymap-path keys handler]
  (let [full (into keymap-path keys)]
    (loop [s state prefix keymap-path remaining keys]
      (cond
        (empty? remaining) (assoc-in s full handler)
        :else (let [cur (get-in s prefix)
                    s'  (if (or (nil? cur) (map? cur))
                          s
                          (assoc-in s prefix {}))]
                (recur s' (conj prefix (first remaining)) (rest remaining)))))))

(defn- el-global-set-key [key-str func-sym]
  (let [keys (parse-key-string key-str)]
    (swap! *state* bind-key [:el-bindings] keys (make-handler func-sym))))

;; --- Mode bridge ---

(defn- el-define-key [mode-name key-str func-sym]
  (let [keys (parse-key-string key-str)]
    (swap! *state* bind-key [:modes mode-name :keymap] keys (make-handler func-sym))))

(defn- el-add-hook [hook-name func-sym]
  (swap! *state* mode/add-hook hook-name (make-handler func-sym))
  hook-name)

(defn- el-run-hooks [hook-name]
  (swap! *state* mode/run-hooks hook-name)
  nil)

(defn- el-set-major-mode [mode-name]
  (swap! *state* mode/set-major-mode mode-name)
  mode-name)

(defn- el-toggle-minor-mode [mode-name]
  (swap! *state* mode/toggle-minor-mode mode-name)
  mode-name)

;; --- Advice ---

(defn- kwify
  "Accept both elisp `:before`/`:after` (parsed as colon-prefixed symbols)
   and Clojure keywords; return a Clojure keyword."
  [k]
  (cond
    (keyword? k) k
    (symbol? k)  (let [n (name k)]
                   (keyword (if (.startsWith n ":") (subs n 1) n)))
    :else        k))

(defn- el-add-advice [target kind advice-sym]
  (swap! *state* update-in [:advice target (kwify kind)]
         (fnil conj []) advice-sym)
  advice-sym)

(defn- el-remove-advice [target advice-sym]
  (swap! *state* update-in [:advice target]
    (fn [a]
      (when a
        (into {} (map (fn [[k v]] [k (vec (remove #{advice-sym} v))]) a)))))
  advice-sym)

;; --- Overlays ---

(defn- el-make-overlay [from to]
  (let [id  (inc (or (:overlay-seq @*state*) 0))
        buf (:buf @*state*)
        o   (ov/make from to :default {:id id :buffer buf})]
    (swap! *state* (fn [s]
                     (-> s (assoc :overlay-seq id)
                           (assoc-in [:overlay-home id] buf)
                           (update-in [:bufs buf :overlays] (fnil conj []) o))))
    id))

(defn- find-overlay
  "Return [buffer-name index] for the overlay with `id`, or nil.
   Looks in the overlay's recorded :buffer first, then falls back to scanning."
  [state id]
  (let [scan #(first (keep-indexed (fn [i ov] (when (= id (:id ov)) i)) %))
        home (get-in state [:overlay-home id])]
    (or (when-let [i (scan (get-in state [:bufs home :overlays]))]
          [home i])
        (some (fn [[n b]]
                (when-let [i (scan (:overlays b))] [n i]))
              (:bufs state)))))

(defn- el-overlay-put [id prop value]
  ;; Re-locate the overlay inside the swap fn so a concurrent mutation
  ;; (nREPL / websocket) between `find-overlay` and `swap!` cannot make
  ;; `idx` point at a different (or missing) overlay.
  (let [k (kwify prop)
        v (if (= k :face) (kwify value) value)]
    (swap! *state*
           (fn [s]
             (if-let [[buf idx] (find-overlay s id)]
               (assoc-in s [:bufs buf :overlays idx k] v)
               s))))
  value)

(defn- el-delete-overlay [id]
  (swap! *state*
         (fn [s]
           (if-let [[buf idx] (find-overlay s id)]
             (-> s (update-in [:bufs buf :overlays]
                              (fn [ovs] (vec (concat (subvec ovs 0 idx)
                                                      (subvec ovs (inc idx))))))
                   (update :overlay-home dissoc id))
             s)))
  nil)

;; --- Package manager bridge ---
;; Uses runtime `requiring-resolve` to avoid a compile-time cycle: xmas.pkg
;; requires xmas.el for eval-string.

(defn- el-package-install [name url]
  (let [install (requiring-resolve 'xmas.pkg/install)
        {:keys [status msg]} (install (clojure.core/name name) url)]
    (swap! *state* assoc :msg msg)
    (= :ok status)))

(defn- el-package-load [name]
  (let [load-pkg (requiring-resolve 'xmas.pkg/load-package)]
    (load-pkg (clojure.core/name name) *state*)))

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
   'global-set-key  el-global-set-key
   'call-original   (fn [] (when *original* (*original*)))
   ;; modes
   'define-key       el-define-key
   'add-hook         el-add-hook
   'run-hooks        el-run-hooks
   'set-major-mode   el-set-major-mode
   'toggle-minor-mode el-toggle-minor-mode
   ;; advice
   'add-advice       el-add-advice
   'remove-advice    el-remove-advice
   ;; overlays
   'make-overlay     el-make-overlay
   'overlay-put      el-overlay-put
   'delete-overlay   el-delete-overlay
   ;; packages
   'package-install  el-package-install
   'package-load     el-package-load
   ;; errors / non-local exits
   'signal           el-signal
   'error            el-error
   'throw            el-throw
   ;; symbols
   'symbol-function  el-symbol-function
   'symbol-value     el-symbol-value
   'symbol-name      (fn [s] (name s))
   'boundp           el-boundp
   'fboundp          el-fboundp
   'fset             el-fset
   'set              el-set
   'makunbound       el-makunbound
   'consp            seq?
   'atom             (fn [x] (not (and (seq? x) (seq x))))
   ;; plists
   'put              el-put
   'get              el-get})

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
    (cond
      (= form 't) true
      ;; keyword-like symbols (`:before`, `:after`) are self-evaluating
      (.startsWith (name form) ":") form
      :else
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
        quasiquote (eval-quasi (first args) 0)
        unquote (err "unquote outside backquote")
        unquote-splicing (err "unquote-splicing outside backquote")
        if      (eval-if args)
        cond    (eval-cond args)
        progn   (eval-body args)
        setq    (eval-setq args)
        let     (eval-let args)
        let*    (eval-let* args)
        defun   (eval-defun args)
        defmacro (eval-defmacro args)
        define-derived-mode (eval-define-mode :major args)
        define-minor-mode   (eval-define-mode :minor args)
        defcustom           (eval-defcustom args)
        defgroup            (eval-defgroup args)
        defvar              (eval-defvar args)
        lambda  {:args (vec (first args)) :body (rest args)}
        while   (eval-while args)
        and     (eval-and args)
        or      (eval-or args)
        condition-case   (eval-condition-case args)
        unwind-protect   (eval-unwind-protect args)
        catch            (eval-catch args)
        ;; function call — check macro table first
        (if-let [mac (and *macros* (get @*macros* head))]
          (eval (apply-user-fn mac args))
          (let [evaled-args (mapv eval args)
                f (or (get @*fns* head)
                      (get builtins head)
                      (err (str "Void function: " head)))]
            (cond
              (contains? *advising* head)            (call-target f evaled-args)
              (seq (get-in @*state* [:advice head])) (apply-with-advice head f evaled-args)
              :else                                  (call-target f evaled-args))))))

    :else (err (str "Cannot eval: " (pr-str form)))))

;; ============================================================
;; PUBLIC API
;; ============================================================

(defn- ensure-env! [editor-state]
  (swap! editor-state
         #(-> % (update :el-vars   (fn [x] (or x (atom {}))))
                (update :el-fns    (fn [x] (or x (atom {}))))
                (update :el-macros (fn [x] (or x (atom {})))))))

(defn- with-el-env
  "Run f with elisp dynamic scope bound from editor-state's shared env atoms.
   (User keybindings go straight onto :el-bindings via `global-set-key`.)"
  [editor-state f]
  (ensure-env! editor-state)
  (binding [*vars*   (:el-vars   @editor-state)
            *fns*    (:el-fns    @editor-state)
            *macros* (:el-macros @editor-state)
            *state*  editor-state]
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

(defn call-fn
  "Apply a user-defined elisp function by symbol, with no args, against the
   given editor-state atom. Returns the function's return value or nil if
   the function is undefined."
  [sym editor-state]
  (ensure-env! editor-state)
  (when-let [elisp-fn (get @(:el-fns @editor-state) sym)]
    (with-el-env editor-state #(apply-user-fn elisp-fn []))))

(defn list-fn-names
  "Return a sorted seq of user-defined elisp function names (symbols) in the
   given editor state value."
  [state]
  (if-let [fns-atom (:el-fns state)]
    (sort (keys @fns-atom))
    []))
