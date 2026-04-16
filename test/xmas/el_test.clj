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

;; ============================================================
;; LEXICAL CLOSURES
;; ============================================================

(deftest closure-basic
  (is (= 1 (ev "(let ((x 1)) (funcall (lambda () x)))"))))

(deftest closure-counter
  (is (= 3 (ev "(progn
                   (let ((n 0))
                     (setq inc-fn (lambda () (setq n (1+ n)))))
                   (funcall inc-fn)
                   (funcall inc-fn)
                   (funcall inc-fn))"))))

(deftest closure-make-adder
  (is (= 8 (ev "(progn
                   (defun make-adder (n) (lambda (x) (+ n x)))
                   (funcall (make-adder 5) 3))"))))

(deftest closure-shared-state
  (is (= 1 (ev "(progn
                   (let ((n 0))
                     (setq inc-fn (lambda () (setq n (1+ n))))
                     (setq get-fn (lambda () n)))
                   (funcall inc-fn)
                   (funcall get-fn))"))))

(deftest closure-defun-in-let
  (is (= 2 (ev "(progn
                   (let ((count 0))
                     (defun bump () (setq count (1+ count))))
                   (bump) (bump))"))))

(deftest closure-nested-let
  (is (= 3 (ev "(let ((x 1))
                   (let ((y 2))
                     (funcall (lambda () (+ x y)))))"))))

(deftest closure-does-not-leak
  ;; let-bound x should not be visible after let exits
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(progn (let ((x 1)) x) x)"))))

;; ============================================================
;; ERROR HANDLING
;; ============================================================

(deftest condition-case-catches-error
  (is (= 42 (ev "(condition-case nil (error \"boom\") (error 42))"))))

(deftest condition-case-binds-error-var
  (is (= "boom" (ev "(condition-case e (error \"boom\") (error (car (cdr e))))"))))

(deftest condition-case-no-error
  (is (= 42 (ev "(condition-case nil 42 (error 99))"))))

(deftest condition-case-specific-condition
  (is (= 42 (ev "(condition-case nil (signal 'my-error '(\"data\")) (my-error 42))"))))

(deftest condition-case-error-catches-all
  (is (= 42 (ev "(condition-case nil (signal 'my-error '(\"data\")) (error 42))"))))

(deftest condition-case-rethrows-unhandled
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(condition-case nil (signal 'my-error '(\"data\")) (other-error 42))"))))

(deftest unwind-protect-cleanup-on-success
  (is (= 11 (ev "(progn
                    (setq x 0)
                    (unwind-protect (setq x 1) (setq x (+ x 10)))
                    x)"))))

(deftest unwind-protect-cleanup-on-error
  (is (= 99 (ev "(progn
                    (setq x 0)
                    (condition-case nil
                      (unwind-protect (error \"boom\") (setq x 99))
                      (error nil))
                    x)"))))

(deftest unwind-protect-returns-body-value
  (is (= 42 (ev "(unwind-protect 42 1 2 3)"))))

(deftest eval-signal-builtin
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(signal 'test-error '(\"hello\"))"))))

;; --- ignore-errors (subr.el) ---

(deftest subr-ignore-errors-suppresses
  (is (nil? (ev-with-subr "(ignore-errors (error \"boom\"))"))))

(deftest subr-ignore-errors-returns-value
  (is (= 42 (ev-with-subr "(ignore-errors 42)"))))

;; ============================================================
;; BUFFER-LOCAL VARIABLES
;; ============================================================

(deftest buffer-local-make-and-read
  (is (= 42 (ev "(progn (setq x 42) (make-local-variable 'x) x)"))))

(deftest buffer-local-setq-writes-local
  (is (= 99 (ev "(progn
                    (setq x 1)
                    (make-local-variable 'x)
                    (setq x 99)
                    x)"))))

(deftest buffer-local-does-not-affect-global
  (let [ed (make-editor)]
    (el/eval-string "(setq x 10)" ed)
    (el/eval-string "(make-local-variable 'x)" ed)
    (el/eval-string "(setq x 99)" ed)
    ;; x in buffer is 99, but global default is still 10
    (is (= 99 (el/eval-1 "x" ed)))
    ;; global can be read via default-value pattern:
    ;; for now just verify local-variable-p
    (is (el/eval-1 "(local-variable-p 'x)" ed))))

(deftest buffer-local-p-false-when-not-local
  (is (not (ev "(progn (setq y 1) (local-variable-p 'y))"))))

(deftest make-variable-buffer-local-auto
  (is (= 42 (ev "(progn
                    (make-variable-buffer-local 'auto-var)
                    (setq auto-var 42)
                    auto-var)"))))

(deftest buffer-local-value-reads-other-buffer
  (let [ed (atom {:buf "*a*"
                  :bufs {"*a*" (assoc (buf/make "*a*" "" nil) :point 0 :locals {'x 10})
                         "*b*" (assoc (buf/make "*b*" "" nil) :point 0 :locals {'x 20})}
                  :kill [] :msg nil :mini nil :scroll 0 :rows 24 :cols 80})]
    (is (= 20 (el/eval-1 "(buffer-local-value 'x \"*b*\")" ed)))))

(deftest symbol-name-builtin
  (is (= "foo" (ev "(symbol-name 'foo)"))))

;; ============================================================
;; HOOKS
;; ============================================================

(deftest add-hook-and-run
  (is (= 42 (ev "(progn
                    (setq result 0)
                    (defun my-hook-fn () (setq result 42))
                    (add-hook 'test-hook 'my-hook-fn)
                    (run-hooks 'test-hook)
                    result)"))))

(deftest add-hook-multiple-fns
  (is (= 3 (ev "(progn
                   (setq counter 0)
                   (defun inc1 () (setq counter (1+ counter)))
                   (defun inc2 () (setq counter (+ counter 2)))
                   (add-hook 'test-hook 'inc1)
                   (add-hook 'test-hook 'inc2)
                   (run-hooks 'test-hook)
                   counter)"))))

(deftest remove-hook-works
  (is (= 0 (ev "(progn
                   (setq counter 0)
                   (defun inc1 () (setq counter (1+ counter)))
                   (add-hook 'test-hook 'inc1)
                   (remove-hook 'test-hook 'inc1)
                   (run-hooks 'test-hook)
                   counter)"))))

(deftest run-hooks-no-hook-defined
  (is (nil? (ev "(run-hooks 'nonexistent-hook)"))))

(deftest run-hook-with-args-works
  (is (= 10 (ev "(progn
                    (setq result 0)
                    (defun adder (n) (setq result (+ result n)))
                    (add-hook 'test-hook 'adder)
                    (run-hook-with-args 'test-hook 10)
                    result)"))))

;; --- define-minor-mode (subr.el) ---

(deftest subr-define-minor-mode-creates-var
  (let [ed (make-editor)]
    (ev-with-subr "(define-minor-mode test-mode \"A test mode\")" ed)
    (is (nil? (el/eval-1 "test-mode" ed)))))

(deftest subr-define-minor-mode-toggle
  (let [ed (make-editor)]
    (ev-with-subr "(define-minor-mode test-mode \"A test mode\")" ed)
    (is (el/eval-1 "(test-mode)" ed))
    (is (not (el/eval-1 "(test-mode)" ed)))))

(deftest subr-define-minor-mode-hook-runs
  (let [ed (make-editor)]
    (ev-with-subr "(progn
                     (define-minor-mode test-mode \"A test mode\")
                     (setq hook-ran nil)
                     (defun on-test () (setq hook-ran t))
                     (add-hook 'test-mode-hook 'on-test)
                     (test-mode))" ed)
    (is (el/eval-1 "hook-ran" ed))))

;; ============================================================
;; KEYMAPS
;; ============================================================

(deftest keymap-make-sparse
  (let [km (ev "(make-sparse-keymap)")]
    (is (map? km))
    (is (:xmas/keymap km))))

(deftest keymap-define-and-lookup
  (is (= 'my-fn (ev "(progn
                        (setq km (make-sparse-keymap))
                        (setq km (define-key km ?a 'my-fn))
                        (lookup-key km ?a))"))))

(deftest keymap-lookup-misses
  (is (nil? (ev "(lookup-key (make-sparse-keymap) ?x)"))))

(deftest keymap-parent-inheritance
  (is (= 'parent-fn (ev "(progn
                            (setq parent (make-sparse-keymap))
                            (setq parent (define-key parent ?a 'parent-fn))
                            (setq child (make-sparse-keymap))
                            (setq child (set-keymap-parent child parent))
                            (lookup-key child ?a))"))))

(deftest keymap-child-overrides-parent
  (is (= 'child-fn (ev "(progn
                           (setq parent (make-sparse-keymap))
                           (setq parent (define-key parent ?a 'parent-fn))
                           (setq child (make-sparse-keymap))
                           (setq child (set-keymap-parent child parent))
                           (setq child (define-key child ?a 'child-fn))
                           (lookup-key child ?a))"))))

(deftest keymapp-predicate
  (is (ev "(keymapp (make-sparse-keymap))"))
  (is (not (ev "(keymapp 42)"))))

(deftest use-local-map-and-current
  (let [ed (make-editor)]
    (el/eval-string "(progn
                       (setq km (make-sparse-keymap))
                       (use-local-map km))" ed)
    (is (el/eval-1 "(keymapp (current-local-map))" ed))))

;; --- interactive ---

(deftest interactive-spec-stored
  (is (ev "(progn
              (defun my-cmd () (interactive) 42)
              (commandp 'my-cmd))")))

(deftest interactive-spec-not-on-normal-fn
  (is (not (ev "(progn (defun my-fn () 42) (commandp 'my-fn))"))))

;; --- define-minor-mode creates keymap ---

(deftest subr-define-minor-mode-creates-keymap
  (let [ed (make-editor)]
    (ev-with-subr "(define-minor-mode test-mode \"Test\")" ed)
    (is (el/eval-1 "(keymapp test-mode-map)" ed))))

(deftest subr-define-minor-mode-is-interactive
  (let [ed (make-editor)]
    (ev-with-subr "(define-minor-mode test-mode \"Test\")" ed)
    (is (el/eval-1 "(commandp 'test-mode)" ed))))

;; ============================================================
;; REGULAR EXPRESSIONS
;; ============================================================

(deftest string-match-basic
  (is (= 3 (ev "(string-match \"lo\" \"hello\")"))))

(deftest string-match-no-match
  (is (nil? (ev "(string-match \"xyz\" \"hello\")"))))

(deftest string-match-with-groups
  (let [ed (make-editor)]
    (el/eval-string "(string-match \"\\\\(hel\\\\)\\\\(lo\\\\)\" \"hello\")" ed)
    (is (= 0 (el/eval-1 "(match-beginning 0)" ed)))
    (is (= "hel" (el/eval-1 "(match-string 1 \"hello\")" ed)))
    (is (= "lo" (el/eval-1 "(match-string 2 \"hello\")" ed)))))

(deftest match-beginning-end
  (let [ed (make-editor)]
    (el/eval-string "(string-match \"l+\" \"hello\")" ed)
    (is (= 2 (el/eval-1 "(match-beginning 0)" ed)))
    (is (= 4 (el/eval-1 "(match-end 0)" ed)))))

(deftest replace-regexp-in-string-basic
  (is (= "heXXo" (ev "(replace-regexp-in-string \"ll\" \"XX\" \"hello\")"))))

(deftest looking-at-buffer
  (let [ed (make-editor "hello world")]
    (is (el/eval-1 "(looking-at \"hel\")" ed))
    (is (not (el/eval-1 "(looking-at \"world\")" ed)))))

(deftest re-search-forward-basic
  (let [ed (make-editor "foo bar baz")]
    (el/eval-string "(re-search-forward \"bar\")" ed)
    (is (= 7 (:point (get (:bufs @ed) "*test*"))))))

(deftest re-search-backward-basic
  (let [ed (make-editor "foo bar baz")]
    (swap! ed assoc-in [:bufs "*test*" :point] 11)
    (el/eval-string "(re-search-backward \"bar\")" ed)
    (is (= 7 (:point (get (:bufs @ed) "*test*"))))))

(deftest match-data-returns-flat-list
  (let [ed (make-editor)]
    (el/eval-string "(string-match \"\\\\(h\\\\)\" \"hello\")" ed)
    (let [md (el/eval-1 "(match-data)" ed)]
      (is (seq? md))
      (is (= 4 (count md))))))

(deftest emacs-regex-literal-parens
  (is (= 0 (ev "(string-match \"(foo)\" \"(foo) bar\")"))))

;; ============================================================
;; TEXT PROPERTIES
;; ============================================================

(deftest put-get-text-property
  (let [ed (make-editor "hello")]
    (el/eval-string "(put-text-property 0 5 'face 'bold)" ed)
    (is (= 'bold (el/eval-1 "(get-text-property 0 'face)" ed)))))

(deftest text-properties-at-pos
  (let [ed (make-editor "hello")]
    (el/eval-string "(put-text-property 0 5 'face 'bold)" ed)
    (let [props (el/eval-1 "(text-properties-at 2)" ed)]
      (is (seq? props))
      (is (some #(= % 'face) props)))))

(deftest next-property-change-basic
  (let [ed (make-editor "hello world")]
    (el/eval-string "(put-text-property 0 5 'face 'bold)" ed)
    (is (= 5 (el/eval-1 "(next-property-change 0)" ed)))))

(deftest remove-text-properties-basic
  (let [ed (make-editor "hello")]
    (el/eval-string "(put-text-property 0 5 'face 'bold)" ed)
    (el/eval-string "(remove-text-properties 0 5 '(face nil))" ed)
    (is (nil? (el/eval-1 "(get-text-property 0 'face)" ed)))))

;; ============================================================
;; HASH TABLES
;; ============================================================

(deftest hash-table-create-and-put-get
  (is (= 42 (ev "(progn
                    (setq ht (make-hash-table))
                    (puthash 'key 42 ht)
                    (gethash 'key ht))"))))

(deftest hash-table-default-value
  (is (= 99 (ev "(progn
                    (setq ht (make-hash-table))
                    (gethash 'missing ht 99))"))))

(deftest hash-table-remhash
  (is (nil? (ev "(progn
                   (setq ht (make-hash-table))
                   (puthash 'a 1 ht)
                   (remhash 'a ht)
                   (gethash 'a ht))"))))

(deftest hash-table-count-works
  (is (= 2 (ev "(progn
                   (setq ht (make-hash-table))
                   (puthash 'a 1 ht)
                   (puthash 'b 2 ht)
                   (hash-table-count ht))"))))

(deftest hash-table-p-predicate
  (is (ev "(hash-table-p (make-hash-table))"))
  (is (not (ev "(hash-table-p 42)"))))

(deftest hash-table-keys-values
  (let [ed (make-editor)]
    (el/eval-string "(progn (setq ht (make-hash-table))
                            (puthash 'a 1 ht)
                            (puthash 'b 2 ht))" ed)
    (is (= 2 (count (el/eval-1 "(hash-table-keys ht)" ed))))
    (is (= 2 (count (el/eval-1 "(hash-table-values ht)" ed))))))

(deftest hash-table-maphash
  (is (= 3 (ev "(progn
                   (setq ht (make-hash-table))
                   (puthash 'a 1 ht)
                   (puthash 'b 2 ht)
                   (setq sum 0)
                   (maphash (lambda (k v) (setq sum (+ sum v))) ht)
                   sum)"))))

;; ============================================================
;; AUTOLOAD
;; ============================================================

(deftest autoload-loads-on-call
  (let [dir (System/getProperty "java.io.tmpdir")
        f (java.io.File. dir "auto-test.el")]
    (try
      (spit f "(defun auto-fn () 42) (provide 'auto-test)")
      (let [ed (make-editor)]
        (el/eval-string (str "(progn (setq load-path '(\"" dir "\"))
                                     (autoload 'auto-fn \"auto-test\"))") ed)
        (is (= 42 (el/eval-1 "(auto-fn)" ed))))
      (finally (.delete f)))))

;; ============================================================
;; INTERACTIVE
;; ============================================================

(deftest interactive-p-false-normally
  (is (not (ev "(progn (defun f () (interactive-p)) (f))"))))

(deftest call-interactively-no-spec
  (is (= 42 (ev "(progn (defun my-cmd () (interactive) 42)
                         (call-interactively 'my-cmd))"))))

(deftest call-interactively-region-spec
  (let [ed (make-editor "hello world")]
    (swap! ed assoc-in [:bufs "*test*" :mark] 5)
    (el/eval-string "(defun my-cmd (beg end) (interactive \"r\")
                       (setq result (list beg end)))" ed)
    (el/eval-string "(call-interactively 'my-cmd)" ed)
    (is (= '(0 5) (el/eval-1 "result" ed)))))

(deftest call-interactively-prefix-spec
  (let [ed (make-editor)]
    (el/eval-string "(setq current-prefix-arg 4)" ed)
    (el/eval-string "(defun my-cmd (n) (interactive \"p\") (setq result n))" ed)
    (el/eval-string "(call-interactively 'my-cmd)" ed)
    (is (= 4 (el/eval-1 "result" ed)))))

(deftest called-interactively-p-true-when-interactive
  (is (ev "(progn (defun my-cmd () (interactive) (called-interactively-p 'any))
                  (call-interactively 'my-cmd))")))

;; ============================================================
;; SET-MATCH-DATA
;; ============================================================

(deftest set-match-data-works
  (let [ed (make-editor)]
    (el/eval-string "(set-match-data '(0 5 1 3))" ed)
    (is (= 0 (el/eval-1 "(match-beginning 0)" ed)))
    (is (= 5 (el/eval-1 "(match-end 0)" ed)))
    (is (= 1 (el/eval-1 "(match-beginning 1)" ed)))
    (is (= 3 (el/eval-1 "(match-end 1)" ed)))))

;; ============================================================
;; TIMERS
;; ============================================================

(deftest timer-create-and-type
  (is (ev "(timerp (run-with-timer 1 nil 'ignore))")))

(deftest timer-cancel
  (is (nil? (ev "(cancel-timer (run-with-timer 999 nil 'ignore))"))))

;; ============================================================
;; PROCESSES
;; ============================================================

(deftest shell-command-to-string-works
  (is (= "hello\n" (ev "(shell-command-to-string \"echo hello\")"))))

(deftest call-process-exit-code
  (is (= 0 (ev "(call-process \"true\")"))))

(deftest call-process-inserts-output
  (let [ed (make-editor "")]
    (el/eval-string "(call-process \"echo\" nil t nil \"hi\")" ed)
    (is (.contains ^String (:text (get (:bufs @ed) "*test*")) "hi"))))

;; ============================================================
;; CL-LIB
;; ============================================================

(defn ev-with-cl
  "Eval with subr.el and cl-lib.el loaded."
  ([s] (ev-with-cl s (make-editor)))
  ([s ed]
   (el/eval-string (slurp (clojure.java.io/resource "xmas/subr.el")) ed)
   (el/eval-string (slurp (clojure.java.io/resource "xmas/cl-lib.el")) ed)
   (el/eval-string s ed)))

(deftest cl-defun-works
  (is (= 6 (ev-with-cl "(progn (cl-defun my-add (a b) (+ a b)) (my-add 2 4))"))))

(deftest cl-case-basic
  (is (= 2 (ev-with-cl "(cl-case 'b (a 1) (b 2) (c 3))"))))

(deftest cl-case-default
  (is (= 99 (ev-with-cl "(cl-case 'z (a 1) (t 99))"))))

(deftest cl-remove-if-works
  (is (= '(1 3 5) (ev-with-cl "(cl-remove-if (lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5))"))))

(deftest cl-remove-if-not-works
  (is (= '(2 4) (ev-with-cl "(cl-remove-if-not (lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5))"))))

(deftest cl-find-if-works
  (is (= 4 (ev-with-cl "(cl-find-if (lambda (x) (> x 3)) '(1 2 3 4 5))"))))

(deftest cl-position-works
  (is (= 2 (ev-with-cl "(cl-position 'c '(a b c d))"))))

(deftest cl-reduce-works
  (is (= 10 (ev-with-cl "(cl-reduce '+ '(1 2 3 4))"))))

(deftest cl-incf-works
  (is (= 3 (ev-with-cl "(progn (setq x 1) (cl-incf x 2) x)"))))

(deftest cl-decf-works
  (is (= 5 (ev-with-cl "(progn (setq x 7) (cl-decf x 2) x)"))))

(deftest cl-loop-for-in-collect
  (is (= '(2 4 6) (ev-with-cl "(cl-loop for x in '(1 2 3) collect (* x 2))"))))

(deftest cl-loop-for-from-to-collect
  (is (= '(1 4 9) (ev-with-cl "(cl-loop for i from 1 to 3 collect (* i i))"))))

(deftest cl-destructuring-bind-basic
  (is (= 6 (ev-with-cl "(cl-destructuring-bind (a b c) '(1 2 3) (+ a b c))"))))

(deftest cl-destructuring-bind-rest
  (is (= '(2 3) (ev-with-cl "(cl-destructuring-bind (a &rest b) '(1 2 3) b)"))))

(deftest cl-block-return-from
  (is (= 42 (ev-with-cl "(cl-block my-block (cl-return-from my-block 42) 99)"))))

(deftest cl-labels-works
  (is (= 6 (ev-with-cl "(cl-labels ((double (x) (* x 2)))
                           (funcall double 3))"))))

;; ============================================================
;; READER: DOTTED PAIRS
;; ============================================================

(deftest read-dotted-pair
  (let [form (r1 "(a . b)")]
    (is (= 'a (first form)))
    (is (instance? xmas.el.DottedPair form))))

(deftest read-dotted-pair-cdr
  (is (= 'b (ev "(cdr '(a . b))"))))

(deftest read-dotted-pair-car
  (is (= 'a (ev "(car '(a . b))"))))

(deftest read-dotted-proper-list
  ;; (a . (b c)) is just (a b c)
  (is (= '(a b c) (r1 "(a . (b c))"))))

(deftest read-dotted-nil
  ;; (a . nil) is just (a)
  (is (= '(a) (r1 "(a . nil)"))))

(deftest read-dotted-alist
  (let [ed (make-editor)]
    (ev-with-subr "(setq alist '((a . 1) (b . 2)))" ed)
    (is (= 1 (ev-with-subr "(cdr (assoc 'a alist))" ed)))
    (is (= 2 (ev-with-subr "(cdr (assoc 'b alist))" ed)))))

(deftest cons-creates-dotted-pair
  (is (instance? xmas.el.DottedPair (ev "(cons 'a 'b)"))))

(deftest cons-with-list-cdr
  ;; (cons 'a '(b c)) is a regular list
  (is (= '(a b c) (ev "(cons 'a '(b c))"))))

(deftest dotted-pair-equal
  (is (ev "(equal '(a . b) '(a . b))")))

(deftest prin1-dotted-pair
  (is (= "(a . b)" (ev "(prin1-to-string '(a . b))"))))

(deftest let-dotted-binding
  ;; (let ((x . 1)) x) — dotted pair binding syntax
  (is (= 1 (ev "(let ((x . 1)) x)"))))

;; ============================================================
;; READER: DISPATCH (#)
;; ============================================================

(deftest read-sharp-quote
  (is (= '(function foo) (r1 "#'foo"))))

(deftest eval-sharp-quote-resolves
  (is (ev "(progn (defun my-fn () 42) (functionp #'my-fn))")))

(deftest read-hex-literal
  (is (= 255 (r1 "#xff"))))

(deftest read-octal-literal
  (is (= 8 (r1 "#o10"))))

(deftest read-binary-literal
  (is (= 10 (r1 "#b1010"))))

(deftest read-propertized-string
  ;; #("hello" 0 5 (face bold)) → just "hello"
  (is (= "hello" (r1 "#(\"hello\" 0 5 (face bold))"))))

;; ============================================================
;; SPECIAL FORMS: catch/throw, defconst, function
;; ============================================================

(deftest catch-throw-basic
  (is (= 42 (ev "(catch 'done (throw 'done 42) 99)"))))

(deftest catch-no-throw
  (is (= 99 (ev "(catch 'done 99)"))))

(deftest catch-throw-nested
  (is (= 'inner (ev "(catch 'outer
                        (catch 'inner
                          (throw 'inner 'inner)
                          'unreachable))"))))

(deftest catch-throw-not-caught-by-condition-case
  ;; condition-case should NOT intercept throw
  (is (= 42 (ev "(catch 'done
                    (condition-case nil
                      (throw 'done 42)
                      (error 'wrong)))"))))

(deftest defconst-always-sets
  (is (= 99 (ev "(progn (setq x 1) (defconst x 99) x)"))))

(deftest eval-function-lambda
  (is (= 42 (ev "(funcall (function (lambda () 42)))"))))

(deftest eval-function-symbol
  (is (ev "(progn (defun my-fn () t) (functionp (function my-fn)))")))

(deftest save-current-buffer-restores
  (let [ed (make-editor)]
    (el/eval-string "(get-buffer-create \"other\")" ed)
    (el/eval-string "(save-current-buffer (set-buffer \"other\"))" ed)
    (is (= "*test*" (el/eval-1 "(current-buffer)" ed)))))

;; ============================================================
;; DIRECT-MAPPING PRIMITIVES
;; ============================================================

(deftest eq-identical
  (is (ev "(eq 'a 'a)"))
  (is (not (ev "(eq 1 1.0)"))))

(deftest type-of-works
  (is (= 'integer (ev "(type-of 42)")))
  (is (= 'string (ev "(type-of \"hi\")")))
  (is (= 'symbol (ev "(type-of 'foo)")))
  (is (= 'cons (ev "(type-of '(1 2))"))))

(deftest math-builtins
  (is (= 8.0 (ev "(expt 2 3)")))
  (is (= 2 (ev "(floor 2.7)")))
  (is (= 3 (ev "(ceiling 2.1)")))
  (is (= 3 (ev "(round 2.5)")))
  (is (= 2 (ev "(truncate 2.9)"))))

(deftest bitwise-builtins
  (is (= 0 (ev "(logand 5 2)")))
  (is (= 7 (ev "(logior 5 2)")))
  (is (= 7 (ev "(logxor 5 2)")))
  (is (= 8 (ev "(ash 1 3)"))))

(deftest string-builtins
  (is (= "HELLO" (ev "(upcase \"hello\")")))
  (is (= "hello" (ev "(downcase \"HELLO\")")))
  (is (ev "(string-prefix-p \"hel\" \"hello\")"))
  (is (ev "(string= \"abc\" \"abc\")"))
  (is (= '("a" "b" "c") (ev "(split-string \"a,b,c\" \",\")"))))

(deftest file-predicates
  (is (ev "(file-exists-p \"/tmp\")"))
  (is (ev "(file-directory-p \"/tmp\")"))
  (is (not (ev "(file-exists-p \"/nonexistent-xmas-test-path\")"))))

(deftest buffer-management
  (let [ed (make-editor)]
    (el/eval-string "(get-buffer-create \"new-buf\")" ed)
    (is (el/eval-1 "(buffer-live-p \"new-buf\")" ed))
    (el/eval-string "(kill-buffer \"new-buf\")" ed)
    (is (not (el/eval-1 "(buffer-live-p \"new-buf\")" ed)))))

(deftest buffer-position-predicates
  (is (ev "(bobp)"))
  (is (ev "(bolp)"))
  (is (ev "(eobp)" (make-editor "")))
  (is (ev "(eolp)" (make-editor ""))))

(deftest getenv-works
  (is (string? (ev "(getenv \"HOME\")"))))

(deftest float-time-works
  (is (number? (ev "(float-time)"))))

;; ============================================================
;; KBD
;; ============================================================

(deftest kbd-single-key
  (is (= [[:ctrl \x]] (ev "(kbd \"C-x\")"))))

(deftest kbd-multi-key
  (is (= [[:ctrl \x] [:ctrl \f]] (ev "(kbd \"C-x C-f\")"))))

(deftest kbd-meta
  (is (= [[:meta \x]] (ev "(kbd \"M-x\")"))))

(deftest kbd-special-names
  (is (= :return (first (ev "(kbd \"RET\")"))))
  (is (= :tab (first (ev "(kbd \"TAB\")"))))
  (is (= \space (first (ev "(kbd \"SPC\")")))))

(deftest kbd-angle-bracket
  (is (= :up (first (ev "(kbd \"<up>\")")))))

;; ============================================================
;; USE-PACKAGE + PACKAGE STUBS + CUSTOM
;; ============================================================

(defn ev-with-all
  "Eval with all bootstrap .el files loaded."
  ([s] (ev-with-all s (make-editor)))
  ([s ed]
   (doseq [f ["xmas/subr.el" "xmas/cl-lib.el" "xmas/package-stubs.el"
               "xmas/custom-stubs.el" "xmas/use-package.el"]]
     (el/eval-string (slurp (clojure.java.io/resource f)) ed))
   (el/eval-string s ed)))

(deftest package-initialize-no-crash
  (ev-with-all "(package-initialize)"))

(deftest custom-set-variables-works
  (let [ed (make-editor)]
    (ev-with-all "(custom-set-variables '(my-custom-var 42))" ed)
    (is (= 42 (el/eval-1 "my-custom-var" ed)))))

(deftest use-package-require-with-config
  (let [ed (make-editor)]
    ;; Provide a feature, then use-package it with :config
    (ev-with-all "(progn
                    (provide 'fake-pkg)
                    (use-package fake-pkg
                      :config
                      (setq fake-pkg-loaded t)))" ed)
    (is (el/eval-1 "fake-pkg-loaded" ed))))

(deftest use-package-custom-sets-vars
  (let [ed (make-editor)]
    (ev-with-all "(progn
                    (provide 'fake-pkg)
                    (use-package fake-pkg
                      :custom
                      ((my-var 99))))" ed)
    (is (= 99 (el/eval-1 "my-var" ed)))))

(deftest use-package-defer-does-not-require
  ;; :defer t should not require immediately (no error for missing pkg)
  (ev-with-all "(use-package nonexistent-pkg :defer t)"))

(deftest use-package-hook
  (let [ed (make-editor)]
    (ev-with-all "(progn
                    (provide 'fake-pkg)
                    (use-package fake-pkg
                      :hook
                      ((prog-mode . my-hook-fn))))" ed)
    ;; Verify add-hook was called
    (is (some? (el/eval-1 "prog-mode-hook" ed)))))

;; ============================================================
;; SUBR ADDITIONS
;; ============================================================

(deftest with-current-buffer-works
  (let [ed (make-editor)]
    (ev-with-subr "(get-buffer-create \"other\")" ed)
    (ev-with-subr "(with-current-buffer \"other\" (insert \"hi\"))" ed)
    (is (= "*test*" (el/eval-1 "(current-buffer)" ed)))))

(deftest define-derived-mode-basic
  (let [ed (make-editor)]
    (ev-with-subr "(define-derived-mode my-mode fundamental-mode \"My\")" ed)
    (is (el/eval-1 "(fboundp 'my-mode)" ed))))

(deftest alist-get-works
  (is (= 2 (ev-with-subr "(alist-get 'b '((a . 1) (b . 2) (c . 3)))"))))

(deftest ensure-list-works
  (is (= '(42) (ev-with-subr "(ensure-list 42)")))
  (is (= '(1 2) (ev-with-subr "(ensure-list '(1 2))"))))

;; ============================================================
;; BUG REGRESSION TESTS
;; ============================================================

(deftest capitalize-single-char
  (is (= "A" (ev "(capitalize \"a\")"))))

(deftest capitalize-empty-string
  (is (= "" (ev "(capitalize \"\")"))))

(deftest capitalize-normal
  (is (= "Hello" (ev "(capitalize \"hELLO\")"))))

(deftest logb-zero
  (is (number? (ev "(logb 0)"))))

(deftest logb-normal
  (is (= 3 (ev "(logb 10)"))))

(deftest kbd-bare-modifier
  ;; Should not crash on edge cases
  (is (vector? (ev "(kbd \"C-x\")")))
  (is (vector? (ev "(kbd \"M-x\")"))))

(deftest re-search-bound-past-end
  ;; bound larger than buffer should not crash
  (let [ed (make-editor "hello")]
    (is (nil? (el/eval-1 "(re-search-forward \"xyz\" 999)" ed)))))

(deftest rest-without-var-errors
  (is (thrown? clojure.lang.ExceptionInfo
        (ev "(defun bad (&rest) nil)"))))
