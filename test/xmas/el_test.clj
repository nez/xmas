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

(deftest read-nan-infinity-as-symbols
  ;; Regression: Double/parseDouble accepts NaN and Infinity, which used
  ;; to swallow elisp symbols of those names and turn them into floats.
  (is (= 'NaN (r1 "NaN")))
  (is (= 'Infinity (r1 "Infinity")))
  (is (= '+Infinity (r1 "+Infinity"))))

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

(deftest eval-cond-bodyless-clause-returns-test-value
  ;; Regression: `(cond (X))` should return X's evaluated value when X is
  ;; truthy, not nil — standard elisp fall-through idiom.
  (is (= 42 (ev "(cond (42))")))
  (is (= 7  (ev "(cond (nil 1) (7))")))
  (is (nil? (ev "(cond (nil))"))))

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

;; --- Regression tests ---

(deftest eval-empty-list-is-nil
  ;; Elisp: () evaluates to nil, not a "Void function: nil" error.
  (is (nil? (ev "()")))
  (is (nil? (ev "(if t () 1)"))))

(deftest eval-listp-nil
  ;; Elisp: (listp nil) ⇒ t. Was returning nil because backing predicate was seq?.
  (is (true? (ev "(listp nil)")))
  (is (true? (ev "(listp '(1 2))")))
  (is (not (ev "(listp 42)"))))

(deftest eval-let-parallel
  ;; Elisp `let` (not let*) binds in parallel: init forms see the OUTER env.
  (is (= 1 (ev "(progn (setq x 1) (let ((x 2) (y x)) y))"))))

(deftest eval-let-bare-symbol-binding
  ;; Elisp: (let (x) ...) binds x to nil.
  (is (nil? (ev "(let (x) x)")))
  (is (= 5 (ev "(let (x y) (setq x 5) x)"))))

(deftest eval-search-forward-finds-at-point
  ;; Was starting the scan at (inc point), missing a match at point itself.
  (let [ed (make-editor "world hello world")]
    (el/eval-string "(search-forward \"world\")" ed)
    (is (= 5 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-forward-char-negative-moves-back
  ;; (forward-char -1) should move backward, not no-op.
  (let [ed (make-editor "hello")]
    (swap! ed assoc-in [:bufs "*test*" :point] 3)
    (el/eval-string "(forward-char -2)" ed)
    (is (= 1 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-backward-char-negative-moves-forward
  (let [ed (make-editor "hello")]
    (el/eval-string "(backward-char -3)" ed)
    (is (= 3 (:point (get (:bufs @ed) "*test*"))))))

(deftest eval-global-set-key
  (let [ed (make-editor "hello")]
    (el/eval-string
      "(progn (defun my-cmd () (insert \"!\")) (global-set-key \"\\\\C-ct\" 'my-cmd))"
      ed)
    ;; Verify a binding was registered
    (is (some? (:el-bindings @ed)))))

(deftest eval-global-set-key-handler-runs-elisp
  ;; Regression: the registered handler's `(binding [*vars* *vars* ...])`
  ;; evaluated the RHS at call time — outside any outer binding — so
  ;; *vars*/*fns* rebound to nil and every subsequent var/fn lookup NPE'd.
  (let [ed (make-editor "hello")]
    (el/eval-string
      "(progn (defun my-cmd () (insert \"!\")) (global-set-key \"\\\\C-t\" 'my-cmd))"
      ed)
    (let [handler (get-in @ed [:el-bindings [:ctrl \t]])
          new-state (handler @ed)]
      (is (= "!hello" (str (:text (get (:bufs new-state) "*test*")))))
      (is (= 1 (:point (get (:bufs new-state) "*test*")))))))

(deftest eval-global-set-key-picks-up-redefined-fn
  ;; Regression: make-handler used to resolve the elisp fn at bind time,
  ;; so a later `defun` with the same name didn't take effect on the
  ;; already-bound key. Emacs resolves on each invocation.
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun my-cmd () (insert \"A\")) (global-set-key \"\\\\C-t\" 'my-cmd))"
      ed)
    (el/eval-string "(defun my-cmd () (insert \"B\"))" ed)
    (let [handler (get-in @ed [:el-bindings [:ctrl \t]])
          new-state (handler @ed)]
      (is (= "B" (str (:text (get (:bufs new-state) "*test*"))))))))

;; --- Modes ---

(deftest eval-define-derived-mode
  (let [ed (make-editor "hi")]
    (el/eval-string "(define-derived-mode my-mode nil \"MyMode\")" ed)
    (is (= :major (get-in @ed [:modes 'my-mode :type])))))

(deftest eval-define-minor-mode
  (let [ed (make-editor "hi")]
    (el/eval-string "(define-minor-mode my-minor \"docs\")" ed)
    (is (= :minor (get-in @ed [:modes 'my-minor :type])))))

(deftest eval-define-key-and-dispatch
  (let [ed (make-editor "hi")]
    (el/eval-string
      "(progn (defun bang () (insert \"!\"))
              (define-derived-mode my-mode nil \"MyMode\")
              (define-key 'my-mode \"\\\\C-c\\\\C-c\" 'bang)
              (set-major-mode 'my-mode))"
      ed)
    (let [handler (get-in @ed [:modes 'my-mode :keymap [:ctrl \c] [:ctrl \c]])]
      (is (fn? handler))
      (is (= "!hi" (str (:text (get (:bufs (handler @ed)) "*test*"))))))))

(deftest eval-add-hook-runs-on-mode-activation
  (let [ed (make-editor "hi")]
    (el/eval-string
      "(progn (defun note () (insert \"note \"))
              (define-derived-mode my-mode nil \"MyMode\")
              (add-hook 'my-mode-hook 'note)
              (set-major-mode 'my-mode))"
      ed)
    (is (= "note hi" (str (:text (get (:bufs @ed) "*test*")))))))

;; --- Overlays ---

(deftest eval-make-overlay-returns-id
  (let [ed (make-editor "hello")
        id (el/eval-string "(make-overlay 1 3)" ed)]
    (is (pos? id))
    (is (= 1 (count (:overlays (get (:bufs @ed) "*test*")))))))

(deftest eval-overlay-put-sets-face
  (let [ed (make-editor "hello")]
    (el/eval-string "(let ((o (make-overlay 1 3))) (overlay-put o 'face 'region))" ed)
    (is (= :region (:face (first (:overlays (get (:bufs @ed) "*test*"))))))))

(deftest eval-delete-overlay-removes-it
  (let [ed (make-editor "hello")]
    (el/eval-string "(let ((o (make-overlay 1 3))) (delete-overlay o))" ed)
    (is (empty? (:overlays (get (:bufs @ed) "*test*"))))))

(deftest overlay-moves-with-edits
  (let [ed (make-editor "hello")]
    (el/eval-string "(make-overlay 2 4)" ed)
    ;; insert "XXX" at position 0 → overlay shifts by 3
    (el/eval-string "(goto-char 0) (insert \"XXX\")" ed)
    (let [ov (first (:overlays (get (:bufs @ed) "*test*")))]
      (is (= 5 (:from ov)))
      (is (= 7 (:to ov))))))

;; --- Advice ---

(deftest eval-add-advice-before-runs-first
  (let [ed (make-editor "hi")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun before-advice () (insert \"B\"))
              (add-advice 'target :before 'before-advice)
              (target))"
      ed)
    ;; buffer initially \"hi\"; point at 0; insert advances point,
    ;; so first advice inserts 'B' at 0, moves to 1; target inserts 'T' at 1
    (is (= "BThi" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-add-advice-after-runs-last
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun after-advice () (insert \"A\"))
              (add-advice 'target :after 'after-advice)
              (target))"
      ed)
    (is (= "TA" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-add-advice-around-can-invoke-original
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun wrap ()
                (insert \"(\")
                (call-original)
                (insert \")\"))
              (add-advice 'target :around 'wrap)
              (target))"
      ed)
    (is (= "(T)" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-add-advice-around-can-skip-original
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun skip () (insert \"S\"))
              (add-advice 'target :around 'skip)
              (target))"
      ed)
    (is (= "S" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-add-advice-around-chain
  ;; Two :around advices. Outer added first → applied outermost.
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun outer () (insert \"[\") (call-original) (insert \"]\"))
              (defun inner () (insert \"(\") (call-original) (insert \")\"))
              (add-advice 'target :around 'outer)
              (add-advice 'target :around 'inner)
              (target))"
      ed)
    (is (= "[(T)]" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-remove-advice-cancels-it
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun target () (insert \"T\"))
              (defun marker () (insert \"M\"))
              (add-advice 'target :before 'marker)
              (remove-advice 'target 'marker)
              (target))"
      ed)
    (is (= "T" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-advice-calling-target-does-not-recurse
  ;; Regression: advice body that calls the advised target would re-enter
  ;; advice and recurse until stack overflow. Advice must run once per
  ;; outer call; re-entry dispatches the raw function.
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun foo () (insert \"F\"))
              (defun bar () (foo) (insert \"B\"))
              (add-advice 'foo :around 'bar)
              (foo))"
      ed)
    ;; :around runs once, bar calls foo (raw, no advice), then appends B.
    (is (= "FB" (str (:text (get (:bufs @ed) "*test*")))))))

(deftest eval-global-set-key-prefix-over-existing-binding
  ;; Regression: binding `C-x C-f` after `C-x` was already a plain command
  ;; used to throw ClassCastException out of `assoc-in`.
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (defun a () nil)
              (defun b () nil)
              (global-set-key \"\\\\C-x\" 'a)
              (global-set-key \"\\\\C-x\\\\C-f\" 'b))"
      ed)
    (let [binds (:el-bindings @ed)]
      (is (map? (get binds [:ctrl \x])))
      (is (fn? (get-in binds [[:ctrl \x] [:ctrl \f]]))))))

;; --- defcustom / defgroup / defvar ---

(deftest eval-defcustom-sets-value
  (let [ed (make-editor "")]
    (el/eval-string "(defcustom my-var 42 \"my doc\")" ed)
    (is (= 42 (get @(:el-vars @ed) 'my-var)))
    (is (= "my doc" (get-in @ed [:custom-docs 'my-var])))))

(deftest eval-defgroup-registers-group
  (let [ed (make-editor "")]
    (el/eval-string "(defgroup my-group nil \"group doc\")" ed)
    (is (= "group doc" (get-in @ed [:custom-groups 'my-group])))))

(deftest eval-defvar-preserves-existing-value
  (let [ed (make-editor "")]
    (el/eval-string "(defvar x 10)" ed)
    (el/eval-string "(setq x 99)" ed)
    (el/eval-string "(defvar x 10)" ed)
    (is (= 99 (get @(:el-vars @ed) 'x)))))

(deftest eval-defvar-no-default-does-not-bind
  ;; Regression: `(defvar x)` without a default used to bind x to nil, so
  ;; (boundp 'x) returned t. Emacs distinguishes declare-only from bind-to-nil.
  (let [ed (make-editor "")]
    (el/eval-string "(defvar forward-decl)" ed)
    (is (not (contains? @(:el-vars @ed) 'forward-decl)))
    (is (false? (el/eval-string "(boundp 'forward-decl)" ed)))))

(deftest eval-toggle-minor-mode
  (let [ed (make-editor "hi")]
    (el/eval-string
      "(progn (define-minor-mode my-minor \"docs\")
              (toggle-minor-mode 'my-minor))"
      ed)
    (is (= #{'my-minor} (:minor-modes (get (:bufs @ed) "*test*"))))))

;; ============================================================
;; Tier 1: Reader — backquote / unquote / unquote-splicing
;; ============================================================

(deftest read-backquote
  (is (= '(quasiquote x) (r1 "`x"))))

(deftest read-unquote
  (is (= '(quasiquote (unquote x)) (r1 "`,x"))))

(deftest read-unquote-splicing
  (is (= '(quasiquote (unquote-splicing x)) (r1 "`,@x"))))

(deftest read-quasi-inside-list
  (is (= '(quasiquote (a (unquote b) c))
         (r1 "`(a ,b c)"))))

(deftest read-quasi-with-splice
  (is (= '(quasiquote (a (unquote-splicing b) c))
         (r1 "`(a ,@b c)"))))

;; ============================================================
;; Tier 1: Macros + defmacro
;; ============================================================

(deftest eval-defmacro-registers
  (let [ed (make-editor "")]
    (el/eval-string "(defmacro my-mac (x) (list (quote +) x 1))" ed)
    (is (contains? @(:el-macros @ed) 'my-mac))))

(deftest eval-macro-expands-at-call
  ;; Macro receives unevaluated args; we expand then eval. Result should be
  ;; `(+ 5 1)` = 6.
  (let [ed (make-editor "")]
    (el/eval-string "(defmacro add1 (x) (list (quote +) x 1))" ed)
    (is (= 6 (el/eval-string "(add1 5)" ed)))))

(deftest eval-macro-preserves-form-not-value
  ;; Args are NOT evaluated before the macro body runs.
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn
         (defmacro quote-first (x y) (list (quote quote) x))
         (setq answer (quote-first the-sym other-sym)))"
      ed)
    (is (= 'the-sym (get @(:el-vars @ed) 'answer)))))

(deftest eval-macro-with-quasiquote
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn
         (defmacro swap-args (a b) `(list ,b ,a))
         (setq r (swap-args 1 2)))"
      ed)
    (is (= '(2 1) (get @(:el-vars @ed) 'r)))))

(deftest eval-quasiquote-literal
  (is (= '(1 2 3) (ev "`(1 2 3)"))))

(deftest eval-quasiquote-with-unquote
  (let [ed (make-editor "")]
    (el/eval-string "(setq x 42)" ed)
    (is (= '(1 42 3) (el/eval-string "`(1 ,x 3)" ed)))))

(deftest eval-quasiquote-with-splice
  (let [ed (make-editor "")]
    (el/eval-string "(setq xs (list 2 3 4))" ed)
    (is (= '(1 2 3 4 5) (el/eval-string "`(1 ,@xs 5)" ed)))))

(deftest eval-macro-implementing-when
  ;; &rest isn't supported yet, so calling the macro throws an arity error
  ;; before `r` is assigned. See the next test for the working single-body
  ;; variant. When &rest lands, rewrite this to assert `r = 3`.
  (let [ed (make-editor "")]
    (try
      (el/eval-string
        "(progn
           (defmacro my-when (test &rest body)
             `(if ,test (progn ,@body) nil))
           (setq r (my-when t 1 2 3)))"
        ed)
      (catch Exception _))
    (is (nil? (get @(:el-vars @ed) 'r)))))

(deftest eval-macro-implementing-when-single-body
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn
         (defmacro my-when (test body) `(if ,test ,body nil))
         (setq r (my-when t 99)))"
      ed)
    (is (= 99 (get @(:el-vars @ed) 'r)))))

;; ============================================================
;; Tier 1: condition-case / unwind-protect / catch / throw
;; ============================================================

(deftest eval-signal-caught-by-handler
  (is (= "caught"
         (ev "(condition-case _ (signal 'my-err (list 1)) (my-err \"caught\"))"))))

(deftest eval-condition-case-binds-variable
  (let [ed (make-editor "")]
    (el/eval-string
      "(setq r (condition-case e (signal 'my-err (list 99)) (my-err e)))"
      ed)
    (is (= '(my-err 99) (get @(:el-vars @ed) 'r)))))

(deftest eval-condition-case-unrelated-passes-through
  (is (thrown? clojure.lang.ExceptionInfo
               (ev "(condition-case _ (signal 'other-err nil) (my-err 1))"))))

(deftest eval-condition-case-error-matches-any
  (is (= "ok"
         (ev "(condition-case _ (signal 'weird nil) (error \"ok\"))"))))

(deftest eval-error-builtin-raises-error
  (is (= "boom"
         (ev "(condition-case _ (error \"boom\") (error \"boom\"))"))))

(deftest eval-unwind-protect-runs-cleanup-on-success
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (setq trace nil)
              (unwind-protect 1
                (setq trace (cons 'cleanup trace))))"
      ed)
    (is (= '(cleanup) (get @(:el-vars @ed) 'trace)))))

(deftest eval-unwind-protect-runs-cleanup-on-signal
  (let [ed (make-editor "")]
    (el/eval-string
      "(progn (setq trace nil)
              (condition-case _
                (unwind-protect (signal 'boom nil)
                  (setq trace 'cleaned))
                (boom nil)))"
      ed)
    (is (= 'cleaned (get @(:el-vars @ed) 'trace)))))

(deftest eval-catch-throw-roundtrip
  (is (= 99 (ev "(catch 'tag (throw 'tag 99))"))))

(deftest eval-catch-normal-exit
  (is (= 5 (ev "(catch 'tag (+ 2 3))"))))

(deftest eval-throw-uncaught-raises
  (is (thrown? clojure.lang.ExceptionInfo
               (ev "(throw 'nope 1)"))))

;; ============================================================
;; Tier 1: Symbols — fset, boundp, fboundp, put/get
;; ============================================================

(deftest eval-boundp-bound
  (let [ed (make-editor "")]
    (el/eval-string "(setq x 1)" ed)
    (is (true? (el/eval-string "(boundp 'x)" ed)))))

(deftest eval-boundp-unbound
  (is (false? (ev "(boundp 'not-defined)"))))

(deftest eval-fboundp-builtin
  (is (true? (ev "(fboundp '+)"))))

(deftest eval-fboundp-user-fn
  (let [ed (make-editor "")]
    (el/eval-string "(defun f () 1)" ed)
    (is (true? (el/eval-string "(fboundp 'f)" ed)))))

(deftest eval-fboundp-macro
  (let [ed (make-editor "")]
    (el/eval-string "(defmacro m (x) x)" ed)
    (is (true? (el/eval-string "(fboundp 'm)" ed)))))

(deftest eval-put-get
  (let [ed (make-editor "")]
    (el/eval-string "(put 'x 'color 'red)" ed)
    (is (= 'red (el/eval-string "(get 'x 'color)" ed)))))

(deftest eval-set-updates-variable
  (let [ed (make-editor "")]
    (el/eval-string "(set 'x 7)" ed)
    (is (= 7 (get @(:el-vars @ed) 'x)))))

(deftest eval-makunbound-removes-variable
  (let [ed (make-editor "")]
    (el/eval-string "(setq x 1)" ed)
    (el/eval-string "(makunbound 'x)" ed)
    (is (not (contains? @(:el-vars @ed) 'x)))))
