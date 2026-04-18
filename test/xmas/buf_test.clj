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

;; --- Redo ---

(deftest redo-after-undo
  (let [b (buf/make "t" "abc" nil)
        edited (buf/edit b 3 3 "d")
        undone (buf/undo edited)
        redone (buf/redo undone)]
    (is (= "abcd" (str (:text redone))))
    (is (= 4 (:point redone)))))

(deftest redo-empty-returns-unchanged
  (let [b (buf/make "t" "hello" nil)]
    (is (= b (buf/redo b)))))

(deftest redo-multi-step
  (let [b (buf/make "t" "abc" nil)
        b1 (buf/edit b 3 3 "d")
        b2 (buf/edit b1 4 4 "e")
        u1 (buf/undo b2)
        u2 (buf/undo u1)
        r1 (buf/redo u2)
        r2 (buf/redo r1)]
    (is (= "abc" (str (:text u2))))
    (is (= "abcd" (str (:text r1))))
    (is (= "abcde" (str (:text r2))))))

(deftest edit-clears-redo
  (let [b (buf/make "t" "abc" nil)
        edited (buf/edit b 3 3 "d")
        undone (buf/undo edited)
        re-edited (buf/edit undone 3 3 "x")]
    (is (empty? (:redo re-edited)))))

(deftest redo-then-undo-restores
  ;; Regression: redo pushed the wrong shape onto :undo, corrupting text
  ;; when the user pressed undo after a redo.
  (let [b   (buf/make "t" "hello" nil)
        e1  (buf/edit b 0 5 "X")          ;; "X"
        u1  (buf/undo e1)                  ;; "hello"
        r1  (buf/redo u1)                  ;; "X"
        u2  (buf/undo r1)]                 ;; should be back to "hello"
    (is (= "X" (str (:text r1))))
    (is (= "hello" (str (:text u2))))))

(deftest redo-then-undo-roundtrips-many
  (let [b  (buf/make "t" "abc" nil)
        b1 (buf/edit b 3 3 "d")
        b2 (buf/edit b1 4 4 "e")
        u1 (buf/undo b2)
        u2 (buf/undo u1)
        r1 (buf/redo u2)
        r2 (buf/redo r1)
        u3 (buf/undo r2)
        u4 (buf/undo u3)]
    (is (= "abcde" (str (:text r2))))
    (is (= "abcd"  (str (:text u3))))
    (is (= "abc"   (str (:text u4))))))

;; --- Undo limit ---

(deftest undo-limit-caps-history
  (let [b (reduce (fn [b i]
                    (buf/edit b i i (str (char (+ 65 (mod i 26))))))
                  (buf/make "t" "" nil)
                  (range 1100))]
    (is (<= (count (:undo b)) buf/undo-limit))))

;; --- :line-states truncation on edit/undo/redo ---

(deftest line-states-truncates-at-edit-line
  ;; Simulate a partially-filled state cache and confirm edit truncates to
  ;; the line of the edit.
  (let [b  (assoc (buf/make "t" "a\nb\nc\nd\ne" nil)
                  :line-states [:s0 :s1 :s2 :s3])
        b' (buf/edit b 4 4 "X")] ; pos 4 is first char of "c" (line 2)
    (is (= [:s0 :s1] (:line-states b')))))

(deftest line-states-fully-truncates-on-line0-edit
  (let [b  (assoc (buf/make "t" "abc\ndef" nil)
                  :line-states [:s0])
        b' (buf/edit b 0 0 "X")]
    (is (= [] (:line-states b')))))

(deftest line-states-truncates-on-undo
  (let [b  (-> (buf/make "t" "abc\ndef\nghi" nil)
               (buf/edit 4 4 "X"))
        b' (assoc b :line-states [:s0 :s1 :s2])
        u  (buf/undo b')]
    (is (= [:s0] (:line-states u)))))

(deftest version-bumps-on-edit-and-undo-redo
  (let [b (buf/make "t" "abc" nil)
        v0 (:version b)
        b1 (buf/edit b 3 3 "d")
        b2 (buf/undo b1)
        b3 (buf/redo b2)]
    (is (= 0 v0))
    (is (> (:version b1) v0))
    (is (> (:version b2) (:version b1)))
    (is (> (:version b3) (:version b2)))))
