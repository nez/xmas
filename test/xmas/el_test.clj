(ns xmas.el-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xmas.buf :as buf]
            [xmas.el :as el]))

(defn r1 [s] (first (el/read-all s)))

;; --- Integers ---

(deftest read-integer
  (is (= 42 (r1 "42"))))

(deftest read-negative-integer
  (is (= -7 (r1 "-7"))))

(deftest read-zero
  (is (= 0 (r1 "0"))))

(deftest read-large-integer
  (is (= 1000000 (r1 "1000000"))))

;; --- Floats ---

(deftest read-float
  (is (= 3.14 (r1 "3.14"))))

(deftest read-float-scientific
  (is (= 1e10 (r1 "1e10"))))

(deftest read-negative-float
  (is (= -2.5 (r1 "-2.5"))))

(deftest read-float-no-leading-digit
  (is (= 0.5 (r1 "0.5"))))

;; --- Strings ---

(deftest read-string-basic
  (is (= "hello" (r1 "\"hello\""))))

(deftest read-string-empty
  (is (= "" (r1 "\"\""))))

(deftest read-string-escapes
  (is (= "a\"b\\c\nd\te" (r1 "\"a\\\"b\\\\c\\nd\\te\""))))

(deftest read-string-with-parens
  (is (= "(foo)" (r1 "\"(foo)\""))))

;; --- Symbols ---

(deftest read-symbol-basic
  (is (= 'foo (r1 "foo"))))

(deftest read-symbol-hyphen
  (is (= 'bar-baz (r1 "bar-baz"))))

(deftest read-symbol-plus
  (is (= '+ (r1 "+"))))

(deftest read-symbol-1plus
  (is (= (symbol "1+") (r1 "1+"))))

(deftest read-symbol-lte
  (is (= '<= (r1 "<="))))

(deftest read-symbol-stringeq
  (is (= 'string= (r1 "string="))))

(deftest read-symbol-predicate
  (is (= 'null? (r1 "null?"))))

(deftest read-symbol-minus
  (is (= '- (r1 "-"))))

(deftest read-symbol-slash
  (is (= '/ (r1 "/"))))

;; --- nil and t ---

(deftest read-nil
  (is (nil? (r1 "nil"))))

(deftest read-nil-returns-in-vector
  (is (= [nil] (el/read-all "nil"))))

(deftest read-t
  (is (= 't (r1 "t"))))

(deftest read-nil-not-prefix
  (is (= 'nilly (r1 "nilly"))))

;; --- Lists ---

(deftest read-list-basic
  (is (= '(a b c) (r1 "(a b c)"))))

(deftest read-list-nested
  (is (= '(a (b c) d) (r1 "(a (b c) d)"))))

(deftest read-list-empty
  (is (= '() (r1 "()"))))

(deftest read-list-mixed-types
  (is (= '(+ 1 2) (r1 "(+ 1 2)"))))

(deftest read-list-with-strings
  (is (= (list 'message "hello %s" 'name) (r1 "(message \"hello %s\" name)"))))

;; --- Vectors ---

(deftest read-vector-basic
  (is (= '[a b c] (r1 "[a b c]"))))

(deftest read-vector-empty
  (is (= [] (r1 "[]"))))

(deftest read-vector-nested
  (is (= '[[1 2] [3 4]] (r1 "[[1 2] [3 4]]"))))

;; --- Quote ---

(deftest read-quote-symbol
  (is (= '(quote x) (r1 "'x"))))

(deftest read-quote-list
  (is (= '(quote (a b)) (r1 "'(a b)"))))

(deftest read-quote-nested
  (is (= '(quote (quote x)) (r1 "''x"))))

;; --- Character literals ---

(deftest read-char-a
  (is (= \a (r1 "?a"))))

(deftest read-char-space
  (is (= \space (r1 "?\\s"))))

(deftest read-char-newline
  (is (= \newline (r1 "?\\n"))))

(deftest read-char-tab
  (is (= \tab (r1 "?\\t"))))

(deftest read-char-backslash
  (is (= \\ (r1 "?\\\\"))))

(deftest read-char-in-list
  (is (= (list 'insert \a) (r1 "(insert ?a)"))))

;; --- Comments ---

(deftest read-with-leading-comment
  (is (= [42] (el/read-all "; comment\n42"))))

(deftest read-with-inline-comment
  (is (= [42] (el/read-all "42 ; comment"))))

(deftest read-comment-only
  (is (= [] (el/read-all "; just a comment\n"))))

;; --- Whitespace ---

(deftest read-ignores-whitespace
  (is (= [1 2 3] (el/read-all "  1  2  3  "))))

(deftest read-handles-newlines
  (is (= [1 2] (el/read-all "1\n2"))))

;; --- Multiple forms ---

(deftest read-all-multiple
  (is (= ['defun 'foo (list 'x) (list '+ 'x 1)]
         (el/read-all "defun foo (x) (+ x 1)"))))

;; --- Full expressions ---

(deftest read-defun
  (is (= '(defun add (a b) (+ a b))
         (r1 "(defun add (a b) (+ a b))"))))

(deftest read-setq
  (is (= '(setq x 42)
         (r1 "(setq x 42)"))))

(deftest read-let
  (is (= '(let ((x 1) (y 2)) (+ x y))
         (r1 "(let ((x 1) (y 2)) (+ x y))"))))

(deftest read-global-set-key
  (is (= (list 'global-set-key "\\C-cd" (list 'quote 'my-func))
         (r1 "(global-set-key \"\\\\C-cd\" 'my-func)"))))

;; --- Errors ---

(deftest error-unmatched-close-paren
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all ")"))))

(deftest error-unmatched-close-bracket
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "]"))))

(deftest error-unterminated-string
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "\"hello"))))

(deftest error-unterminated-list
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "(a b"))))

(deftest error-unterminated-vector
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "[a b"))))

(deftest error-unknown-string-escape
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "\"\\q\""))))

(deftest error-eof-in-string-escape
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "\"\\"))))

(deftest error-eof-after-question-mark
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "?"))))

(deftest error-eof-after-quote
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "'"))))

(deftest error-eof-in-char-escape
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "?\\"))))

(deftest error-unknown-char-escape
  (is (thrown? clojure.lang.ExceptionInfo (el/read-all "?\\q"))))

;; --- Carriage return whitespace ---

(deftest read-handles-carriage-return
  (is (= [1 2] (el/read-all "1\r\n2"))))

;; --- Empty input ---

(deftest read-all-empty
  (is (= [] (el/read-all ""))))

(deftest read-all-whitespace-only
  (is (= [] (el/read-all "   \n  "))))

;; --- Properties ---

(defspec integer-roundtrip 200
  (prop/for-all [n gen/small-integer]
    (= n (r1 (str n)))))

(defspec symbol-roundtrip 200
  (prop/for-all [s (gen/fmap #(apply str %)
                     (gen/not-empty
                       (gen/vector
                         (gen/elements (seq "abcdefghijklmnopqrstuvwxyz-"))
                         1 20)))]
    (let [result (r1 s)]
      (and (symbol? result) (= s (name result))))))

(defspec string-roundtrip 200
  (prop/for-all [s (gen/fmap #(apply str %)
                     (gen/vector (gen/fmap char (gen/choose 32 126)) 0 30))]
    (let [escaped (-> s
                      (.replace "\\" "\\\\")
                      (.replace "\"" "\\\""))
          input (str "\"" escaped "\"")]
      (= s (r1 input)))))

;; ============================================================
;; EVALUATOR TESTS
;; ============================================================

(defn make-editor
  "Create a minimal editor state atom for eval tests."
  ([] (make-editor ""))
  ([text]
   (atom {:buf "*test*"
          :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point 0)}
          :kill [] :msg nil :mini nil :scroll 0 :rows 24 :cols 80})))

(defn ev
  "Eval elisp string against a fresh editor, return result."
  ([s] (ev s (make-editor)))
  ([s editor] (el/eval-string s editor)))

(defn ev-text
  "Eval elisp string, return buffer text."
  ([s] (ev-text s ""))
  ([s initial-text]
   (let [ed (make-editor initial-text)]
     (el/eval-string s ed)
     (str (:text (get (:bufs @ed) (:buf @ed)))))))

(defn ev-point
  "Eval elisp string, return point position."
  ([s] (ev-point s ""))
  ([s initial-text]
   (let [ed (make-editor initial-text)]
     (el/eval-string s ed)
     (:point (get (:bufs @ed) (:buf @ed))))))

;; --- Self-evaluating ---

(deftest eval-number
  (is (= 42 (ev "42"))))

(deftest eval-string
  (is (= "hello" (ev "\"hello\""))))

(deftest eval-nil
  (is (nil? (ev "nil"))))

(deftest eval-t
  (is (true? (ev "t"))))

;; --- Arithmetic ---

(deftest eval-add
  (is (= 7 (ev "(+ 3 4)"))))

(deftest eval-sub
  (is (= 5 (ev "(- 10 5)"))))

(deftest eval-mul
  (is (= 12 (ev "(* 3 4)"))))

(deftest eval-div
  (is (= 5 (ev "(/ 10 2)"))))

(deftest eval-comparison
  (is (true? (ev "(< 1 2)")))
  (is (true? (ev "(> 3 2)")))
  (is (true? (ev "(= 5 5)")))
  (is (true? (ev "(<= 3 3)")))
  (is (true? (ev "(>= 4 3)"))))

;; --- Quote ---

(deftest eval-quote
  (is (= 'foo (ev "'foo")))
  (is (= '(a b c) (ev "'(a b c)"))))

;; --- Variables ---

(deftest eval-setq
  (is (= 42 (ev "(progn (setq x 42) x)"))))

(deftest eval-setq-multiple
  (is (= 20 (ev "(progn (setq x 10 y 20) y)"))))

;; --- If ---

(deftest eval-if-true
  (is (= 1 (ev "(if t 1 2)"))))

(deftest eval-if-false
  (is (= 2 (ev "(if nil 1 2)"))))

(deftest eval-if-no-else
  (is (nil? (ev "(if nil 1)"))))

;; --- Cond ---

(deftest eval-cond
  (is (= 2 (ev "(cond (nil 1) (t 2) (t 3))"))))

(deftest eval-cond-no-match
  (is (nil? (ev "(cond (nil 1))"))))

;; --- Progn ---

(deftest eval-progn
  (is (= 3 (ev "(progn 1 2 3)"))))

;; --- Let ---

(deftest eval-let-basic
  (is (= 30 (ev "(let ((x 10) (y 20)) (+ x y))"))))

(deftest eval-let-restores
  (is (= 99 (ev "(progn (setq x 99) (let ((x 1)) x) x)"))))

;; --- Defun ---

(deftest eval-defun-and-call
  (is (= 25 (ev "(progn (defun square (x) (* x x)) (square 5))"))))

(deftest eval-defun-multi-body
  (is (= 3 (ev "(progn (defun f () 1 2 3) (f))"))))

;; --- Lambda ---

(deftest eval-lambda
  ;; Lambda creates a callable value; in Lisp-2 it lives in the var namespace
  (let [ed (make-editor)]
    (el/eval-string "(setq f (lambda (x) (* x 2)))" ed)
    (is (map? (el/eval-1 "f" ed)))))

;; --- While ---

(deftest eval-while
  (is (= 10 (ev "(progn (setq i 0) (while (< i 10) (setq i (+ i 1))) i)"))))

;; --- And / Or / Not ---

(deftest eval-and
  (is (= 3 (ev "(and 1 2 3)")))
  (is (nil? (ev "(and 1 nil 3)"))))

(deftest eval-or
  (is (= 1 (ev "(or nil 1 2)")))
  (is (nil? (ev "(or nil nil)"))))

(deftest eval-not
  (is (true? (ev "(not nil)")))
  (is (not (ev "(not t)"))))

;; --- List operations ---

(deftest eval-cons
  (is (= '(1 2 3) (ev "(cons 1 '(2 3))"))))

(deftest eval-car-cdr
  (is (= 1 (ev "(car '(1 2 3))")))
  (is (= '(2 3) (ev "(cdr '(1 2 3))"))))

(deftest eval-list
  (is (= '(1 2 3) (ev "(list 1 2 3)"))))

(deftest eval-length
  (is (= 3 (ev "(length '(a b c))"))))

(deftest eval-null
  (is (true? (ev "(null nil)")))
  (is (not (ev "(null '(1))"))))

;; --- String operations ---

(deftest eval-concat
  (is (= "foobar" (ev "(concat \"foo\" \"bar\")"))))

(deftest eval-substring
  (is (= "ell" (ev "(substring \"hello\" 1 4)")))
  (is (= "llo" (ev "(substring \"hello\" 2)"))))

;; --- Predicates ---

(deftest eval-numberp
  (is (true? (ev "(numberp 42)")))
  (is (not (ev "(numberp \"x\")"))))

(deftest eval-stringp
  (is (true? (ev "(stringp \"x\")")))
  (is (not (ev "(stringp 42)"))))

(deftest eval-symbolp
  (is (true? (ev "(symbolp 'foo)")))
  (is (not (ev "(symbolp 42)"))))

(deftest eval-equal
  (is (true? (ev "(equal 1 1)")))
  (is (not (ev "(equal 1 2)"))))

;; --- Buffer bridge ---

(deftest eval-point
  (is (= 0 (ev "(point)"))))

(deftest eval-insert
  (is (= "hello" (ev-text "(insert \"hello\")"))))

(deftest eval-insert-multiple
  (is (= "ab" (ev-text "(insert \"a\" \"b\")"))))

(deftest eval-goto-char
  (is (= 3 (ev-point "(goto-char 3)" "hello"))))

(deftest eval-delete-region
  (is (= "hlo" (ev-text "(delete-region 1 3)" "hello"))))

(deftest eval-buffer-string
  (is (= "hello" (ev "(buffer-string)" (make-editor "hello")))))

(deftest eval-forward-backward-char
  (is (= 2 (ev-point "(progn (forward-char) (forward-char) (point))" "hello")))
  (let [ed (make-editor "hello")]
    (swap! ed assoc-in [:bufs "*test*" :point] 3)
    (el/eval-string "(backward-char)" ed)
    (is (= 2 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-beginning-end-of-line
  (let [ed (make-editor "abc\ndef")]
    (swap! ed assoc-in [:bufs "*test*" :point] 5)
    (el/eval-string "(beginning-of-line)" ed)
    (is (= 4 (:point (get (:bufs @ed) "*test*"))))
    (el/eval-string "(end-of-line)" ed)
    (is (= 7 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-search-forward
  (let [ed (make-editor "hello world")]
    (el/eval-string "(search-forward \"world\")" ed)
    (is (= 11 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-message
  (let [ed (make-editor "")]
    (el/eval-string "(message \"hello %s\" \"world\")" ed)
    (is (= "hello world" (:msg @ed)))))

;; --- Defun + buffer manipulation ---

(deftest eval-defun-inserts
  (is (= "XYZhello"
         (ev-text "(progn (defun prepend () (goto-char 0) (insert \"XYZ\")) (prepend))"
                  "hello"))))

(deftest eval-defun-kill-line
  (is (= "\ndef"
         (ev-text "(progn
                     (defun my-kill-to-eol ()
                       (let ((start (point)))
                         (end-of-line)
                         (delete-region start (point))))
                     (my-kill-to-eol))"
                  "abc\ndef"))))

;; --- Key string parsing ---

(deftest parse-key-ctrl
  (is (= [[:ctrl \c]] (el/parse-key-string "\\C-c"))))

(deftest parse-key-meta
  (is (= [[:meta \x]] (el/parse-key-string "\\M-x"))))

(deftest parse-key-sequence
  (is (= [[:ctrl \c] \d] (el/parse-key-string "\\C-cd"))))

(deftest parse-key-multi-ctrl
  (is (= [[:ctrl \x] [:ctrl \s]] (el/parse-key-string "\\C-x\\C-s"))))

(deftest parse-key-plain
  (is (= [\a \b \c] (el/parse-key-string "abc"))))

;; --- Global-set-key ---

(deftest eval-global-set-key
  (let [ed (make-editor "hello")]
    (el/eval-string
      "(progn (defun my-cmd () (insert \"!\")) (global-set-key \"\\\\C-ct\" 'my-cmd))"
      ed)
    ;; Verify a binding was registered
    (is (some? (:el-bindings @ed)))))

;; ============================================================
;; TIER 0 TESTS
;; ============================================================

;; --- Backquote reader ---

(deftest read-backquote-symbol
  (is (= '(backquote foo) (r1 "`foo"))))

(deftest read-backquote-list
  (is (= '(backquote (a b c)) (r1 "`(a b c)"))))

(deftest read-unquote
  (is (= '(backquote (a (unquote b) c)) (r1 "`(a ,b c)"))))

(deftest read-splice-unquote
  (is (= '(backquote (a (splice-unquote b) c)) (r1 "`(a ,@b c)"))))

;; --- Backquote eval ---

(deftest eval-backquote-literal
  (is (= '(a b c) (ev "`(a b c)"))))

(deftest eval-backquote-unquote
  (is (= '(a 42 c) (ev "(progn (setq x 42) `(a ,x c))"))))

(deftest eval-backquote-splice
  (is (= '(a 1 2 3 c) (ev "(progn (setq xs '(1 2 3)) `(a ,@xs c))"))))

(deftest eval-backquote-nested
  (is (= '(list 1 (+ 2 3)) (ev "`(list 1 (+ 2 3))"))))

(deftest eval-backquote-empty-splice
  (is (= '(a c) (ev "(progn (setq xs nil) `(a ,@xs c))"))))

(deftest eval-backquote-splice-non-list
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(progn (setq x 5) `(a ,@x c))"))))

(deftest eval-backquote-vector
  (is (= [1 2 3] (ev "(progn (setq x 2) `[1 ,x 3])"))))

;; --- &rest / &optional ---

(deftest eval-defun-rest
  (is (= '(2 3) (ev "(progn (defun my-rest (a &rest b) b) (my-rest 1 2 3))"))))

(deftest eval-defun-rest-empty
  (is (nil? (ev "(progn (defun my-rest (a &rest b) b) (my-rest 1))"))))

(deftest eval-defun-optional
  (is (= 10 (ev "(progn (defun my-opt (a &optional b) (if b b a)) (my-opt 10))"))))

(deftest eval-defun-optional-provided
  (is (= 20 (ev "(progn (defun my-opt (a &optional b) (if b b a)) (my-opt 10 20))"))))

(deftest eval-defun-optional-and-rest
  (is (= '(3 4) (ev "(progn (defun my-fn (a &optional b &rest c) c) (my-fn 1 2 3 4))"))))

(deftest eval-lambda-rest
  (is (= '(2 3) (ev "(funcall (lambda (a &rest b) b) 1 2 3)"))))

;; --- defvar / defcustom ---

(deftest eval-defvar-sets-unbound
  (is (= 42 (ev "(progn (defvar my-var 42) my-var)"))))

(deftest eval-defvar-does-not-overwrite
  (is (= 10 (ev "(progn (setq my-var 10) (defvar my-var 42) my-var)"))))

(deftest eval-defvar-no-value
  (is (= 'my-var (ev "(defvar my-var)"))))

(deftest eval-defcustom
  (is (= 4 (ev "(progn (defcustom my-indent 4 \"Indent width\") my-indent)"))))

;; --- let vs let* ---

(deftest eval-let-parallel-binding
  ;; In let, init-forms are evaluated before any binding happens,
  ;; so (b a) should fail because a is not yet bound
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(let ((a 1) (b a)) b)"))))

(deftest eval-let*-sequential
  ;; In let*, each binding sees previous ones
  (is (= 3 (ev "(let* ((a 1) (b (+ a 2))) b)"))))

(deftest eval-let*-sequential-refs
  ;; let* allows referencing earlier bindings
  (is (= 1 (ev "(let* ((a 1) (b a)) b)"))))

(deftest eval-let*-restores
  (is (= 99 (ev "(progn (setq x 99) (let* ((x 1) (y (+ x 1))) y) x)"))))

;; --- defmacro ---

(deftest eval-defmacro-simple
  (is (= 42 (ev "(progn (defmacro my-quote (x) (list 'quote x)) (my-quote 42))"))))

(deftest eval-defmacro-when
  (is (= 1 (ev "(progn (defmacro my-when (c &rest body) `(if ,c (progn ,@body))) (my-when t 1))"))))

(deftest eval-defmacro-when-false
  (is (nil? (ev "(progn (defmacro my-when (c &rest body) `(if ,c (progn ,@body))) (my-when nil 1))"))))

(deftest eval-macroexpand
  (is (= '(if t (progn 1))
         (ev "(progn (defmacro my-when (c &rest body) `(if ,c (progn ,@body))) (macroexpand '(my-when t 1)))"))))

(deftest eval-macro-not-a-macro
  (is (= 42 (ev "(macroexpand 42)"))))

;; --- provide / require / featurep ---

(deftest eval-provide-featurep
  (is (ev "(progn (provide 'my-feat) (featurep 'my-feat))")))

(deftest eval-featurep-missing
  (is (not (ev "(featurep 'nonexistent)"))))

(deftest eval-require-loads-file
  (let [dir (System/getProperty "java.io.tmpdir")
        f (java.io.File. dir "test-mod.el")]
    (try
      (spit f "(provide 'test-mod) (defvar test-mod-loaded t)")
      (let [ed (make-editor)]
        (el/eval-string (str "(progn (setq load-path '(\"" dir "\")) (require 'test-mod))") ed)
        ;; feature should be provided
        (is (contains? @(:el-features @ed) 'test-mod)))
      (finally (.delete f)))))

;; --- save-excursion ---

(deftest eval-save-excursion-restores-point
  (is (= 0 (ev-point "(save-excursion (goto-char 3))" "hello"))))

(deftest eval-save-excursion-returns-body-value
  (is (= 3 (ev "(save-excursion (goto-char 3))" (make-editor "hello")))))

;; --- Property lists ---

(deftest eval-put-get
  (is (= 42 (ev "(progn (put 'my-sym 'my-prop 42) (get 'my-sym 'my-prop))"))))

(deftest eval-get-missing
  (is (nil? (ev "(get 'no-sym 'no-prop)"))))

(deftest eval-plist-get
  (is (= 2 (ev "(plist-get '(a 1 b 2 c 3) 'b)"))))

(deftest eval-plist-get-missing
  (is (nil? (ev "(plist-get '(a 1) 'z)"))))

(deftest eval-plist-put
  (is (= '(a 1 b 99 c 3) (ev "(plist-put '(a 1 b 2 c 3) 'b 99)"))))

;; --- Additional builtins ---

(deftest eval-boundp
  (is (ev "(progn (setq x 1) (boundp 'x))")))

(deftest eval-boundp-false
  (is (not (ev "(boundp 'unbound-xyz)"))))

(deftest eval-fboundp
  (is (ev "(progn (defun my-fn () 1) (fboundp 'my-fn))")))

(deftest eval-funcall
  (is (= 6 (ev "(progn (defun double (x) (* x 2)) (funcall 'double 3))"))))

(deftest eval-funcall-lambda
  (is (= 4 (ev "(funcall (lambda (x) (* x 2)) 2)"))))

(deftest eval-apply
  (is (= 6 (ev "(apply '+ '(1 2 3))"))))

(deftest eval-apply-mixed
  (is (= 10 (ev "(apply '+ 1 2 '(3 4))"))))

(deftest eval-mapcar
  (is (= '(2 4 6) (ev "(mapcar (lambda (x) (* x 2)) '(1 2 3))"))))

(deftest eval-1-plus
  (is (= 6 (ev "(1+ 5)"))))

(deftest eval-1-minus
  (is (= 4 (ev "(1- 5)"))))

(deftest eval-string-to-number
  (is (= 42 (ev "(string-to-number \"42\")"))))

(deftest eval-number-to-string
  (is (= "42" (ev "(number-to-string 42)"))))

(deftest eval-intern
  (is (= 'foo (ev "(intern \"foo\")"))))

(deftest eval-error-throws
  (is (thrown? clojure.lang.ExceptionInfo (ev "(error \"bad %s\" \"thing\")"))))

(deftest eval-listp-nil
  (is (ev "(listp nil)")))

(deftest eval-consp
  (is (ev "(consp '(1 2))")))

(deftest eval-consp-nil
  (is (not (ev "(consp nil)"))))

(deftest eval-reverse
  (is (= '(3 2 1) (ev "(reverse '(1 2 3))"))))

(deftest eval-delete-char
  (is (= "ello" (ev-text "(delete-char)" "hello"))))

;; --- subr.el integration ---

(defn ev-with-subr
  "Eval with subr.el loaded first."
  ([s] (ev-with-subr s (make-editor)))
  ([s ed]
   (el/eval-string (slurp (clojure.java.io/resource "xmas/subr.el")) ed)
   (el/eval-string s ed)))

(deftest subr-when-true
  (is (= 42 (ev-with-subr "(when t 42)"))))

(deftest subr-when-false
  (is (nil? (ev-with-subr "(when nil 42)"))))

(deftest subr-unless-true
  (is (nil? (ev-with-subr "(unless t 42)"))))

(deftest subr-unless-false
  (is (= 42 (ev-with-subr "(unless nil 42)"))))

(deftest subr-dolist
  (is (= 6 (ev-with-subr "(progn (setq sum 0) (dolist (x '(1 2 3)) (setq sum (+ sum x))) sum)"))))

(deftest subr-dotimes
  (is (= 10 (ev-with-subr "(progn (setq sum 0) (dotimes (i 5) (setq sum (+ sum i))) sum)"))))

(deftest subr-push-pop
  (is (= 3 (ev-with-subr "(progn (setq stack nil) (push 1 stack) (push 2 stack) (push 3 stack) (pop stack))"))))

(deftest subr-cadr
  (is (= 2 (ev-with-subr "(cadr '(1 2 3))"))))

(deftest subr-member
  (is (= '(2 3) (ev-with-subr "(member 2 '(1 2 3))"))))

(deftest subr-member-missing
  (is (nil? (ev-with-subr "(member 9 '(1 2 3))"))))

(deftest subr-assoc
  (is (= '(b 2) (ev-with-subr "(assoc 'b '((a 1) (b 2) (c 3)))"))))

(deftest subr-prog1
  (is (= 1 (ev-with-subr "(prog1 1 2 3)"))))

(deftest subr-nthcdr
  (is (= '(3 4) (ev-with-subr "(nthcdr 2 '(1 2 3 4))"))))

(deftest subr-provide
  (let [ed (make-editor)]
    (ev-with-subr "(provide 'subr)" ed)
    (is (ev-with-subr "(featurep 'subr)" ed))))
