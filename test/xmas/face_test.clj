(ns xmas.face-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.face :as face]))

(defn- toks [mode line] (first (face/tokenize mode line :normal)))
(defn- state-after [mode line] (second (face/tokenize mode line :normal)))

(deftest tokenize-fundamental-returns-single-span
  (is (= [[0 5 :default]] (toks :fundamental "hello"))))

(deftest tokenize-fundamental-end-state-normal
  (is (= :normal (state-after :fundamental "hello"))))

(deftest tokenize-empty-line
  (is (= [] (toks :clojure-mode ""))))

(deftest tokenize-plain-line
  (is (= [[0 7 :default]] (toks :clojure-mode "foo bar"))))

(deftest tokenize-line-comment
  (is (= [[0 4 :default] [4 15 :comment]]
         (toks :clojure-mode "foo ; a comment"))))

(deftest tokenize-string
  (is (= [[0 4 :default] [4 9 :string] [9 11 :default]]
         (toks :clojure-mode "foo \"bar\" x"))))

(deftest tokenize-string-with-escape
  (is (= [[0 9 :string]]
         (toks :clojure-mode "\"a\\\"b\\\"c\""))))

(deftest tokenize-unterminated-string-sets-in-string
  (is (= :in-string (state-after :clojure-mode "\"abc"))))

(deftest tokenize-unterminated-string-spans
  (is (= [[0 4 :string]]
         (toks :clojure-mode "\"abc"))))

(deftest tokenize-continuation-line-in-string
  ;; starting in :in-string, the whole line is string (no close)
  (is (= [[[0 10 :string]] :in-string]
         (face/tokenize :clojure-mode "continuing" :in-string))))

(deftest tokenize-continuation-line-closes
  ;; "abc\" + 1" = 8 chars; close at col 3 → [0..4) string, [4..8) default
  (is (= [[[0 4 :string] [4 8 :default]] :normal]
         (face/tokenize :clojure-mode "abc\" + 1" :in-string))))

(deftest tokenize-keyword
  (is (= [[0 1 :default] [1 5 :keyword] [5 6 :default]]
         (toks :clojure-mode "(:foo)"))))

(deftest tokenize-builtin-defn
  (is (= [[0 1 :default] [1 5 :builtin] [5 13 :default]]
         (toks :clojure-mode "(defn foo [])"))))

(deftest tokenize-non-special-symbol-is-default
  (is (= [[0 9 :default]] (toks :clojure-mode "(foo bar)"))))

(deftest tokenize-elisp-defun
  (is (= :builtin (nth (first (filter #(= (first %) 1)
                                       (toks :emacs-lisp-mode "(defun foo ())")))
                       2))))

(deftest tokenize-comment-terminates-scan
  (is (= [[0 4 :default] [4 11 :comment]]
         (toks :clojure-mode "foo ; \"not\""))))

(deftest tokenize-string-before-comment
  (is (= [[0 5 :string] [5 6 :default] [6 10 :comment]]
         (toks :clojure-mode "\"bar\" ; hi"))))
