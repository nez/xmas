(ns xmas.buf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [xmas.buf :as buf]
            [xmas.spec :as spec]))

;; --- Properties ---

(defspec edit-preserves-valid-point 200
  (prop/for-all [[buffer from to repl] (spec/gen-edit-args)]
    (let [b' (buf/edit buffer from to repl)]
      (<= 0 (:point b') (count (:text b'))))))

(defspec edit-sets-modified 200
  (prop/for-all [[buffer from to repl] (spec/gen-edit-args)]
    (:modified (buf/edit buffer from to repl))))

(defspec edit-then-undo-restores-text 200
  (prop/for-all [[buffer from to repl] (spec/gen-edit-args)]
    (let [edited (buf/edit buffer from to repl)
          undone (buf/undo edited)]
      (= (:text buffer) (:text undone)))))

(defspec set-point-clamps 200
  (prop/for-all [buffer spec/gen-buffer]
    (let [b1 (buf/set-point buffer (fn [_ _] -100))
          b2 (buf/set-point buffer (fn [_ _] 999999))]
      (and (= 0 (:point b1))
           (= (count (:text buffer)) (:point b2))))))

;; --- Targeted ---

(deftest edit-insert-at-start
  (let [b (buf/make "t" "hello" nil)
        b' (buf/edit b 0 0 "abc")]
    (is (= "abchello" (str (:text b'))))
    (is (= 3 (:point b')))))

(deftest edit-insert-at-end
  (let [b (assoc (buf/make "t" "hello" nil) :point 5)
        b' (buf/edit b 5 5 "!")]
    (is (= "hello!" (str (:text b'))))
    (is (= 6 (:point b')))))

(deftest edit-insert-at-middle
  (let [b (assoc (buf/make "t" "hello" nil) :point 2)
        b' (buf/edit b 2 2 "XY")]
    (is (= "heXYllo" (str (:text b'))))
    (is (= 4 (:point b')))))

(deftest edit-delete-range
  (let [b (buf/make "t" "hello" nil)
        b' (buf/edit b 1 3 "")]
    (is (= "hlo" (str (:text b'))))))

(deftest edit-replace
  (let [b (buf/make "t" "hello" nil)
        b' (buf/edit b 1 4 "XY")]
    (is (= "hXYo" (str (:text b'))))))

(deftest edit-point-before-range
  (let [b (assoc (buf/make "t" "abcdef" nil) :point 1)
        b' (buf/edit b 3 5 "")]
    (is (= 1 (:point b')))))

(deftest edit-point-after-range
  (let [b (assoc (buf/make "t" "abcdef" nil) :point 5)
        b' (buf/edit b 1 3 "")]
    (is (= 3 (:point b')))))

(deftest undo-empty-returns-unchanged
  (let [b (buf/make "t" "hello" nil)]
    (is (= b (buf/undo b)))))

(deftest undo-restores-point
  (let [b (assoc (buf/make "t" "hello" nil) :point 2)
        b' (buf/edit b 2 2 "X")
        b'' (buf/undo b')]
    (is (= "hello" (str (:text b''))))
    (is (= 2 (:point b'')))))

(deftest multi-undo
  (let [b (buf/make "t" "abc" nil)
        b1 (buf/edit b 3 3 "d")
        b2 (buf/edit b1 4 4 "e")
        u1 (buf/undo b2)
        u2 (buf/undo u1)]
    (is (= "abcd" (str (:text u1))))
    (is (= "abc" (str (:text u2))))))

(deftest edit-records-undo-entry
  (let [b (buf/make "t" "hello" nil)
        b' (buf/edit b 1 3 "X")]
    (is (= 1 (count (:undo b'))))
    (is (= {:from 1 :old "el" :new "X"} (first (:undo b'))))))
