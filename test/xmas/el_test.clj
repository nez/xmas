(ns xmas.el-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
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
