(ns xmas.gap-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.gap :as gap]))

;; --- Construction & roundtrip ---

(deftest of-and-tostring
  (is (= "hello" (str (gap/of "hello"))))
  (is (= "" (str (gap/of "")))))

(deftest length-and-count
  (let [g (gap/of "hello")]
    (is (= 5 (count g)))
    (is (= 5 (.length g)))))

(deftest length-empty
  (is (= 0 (count (gap/of "")))))

;; --- charAt ---

(deftest char-at-basic
  (let [g (gap/of "abcde")]
    (is (= \a (.charAt g 0)))
    (is (= \c (.charAt g 2)))
    (is (= \e (.charAt g 4)))))

(deftest char-at-after-edit
  ;; After insert at pos 2, gap is at pos 4 (2 + len("XY"))
  ;; Content: "abXYcde"
  (let [g (gap/edit (gap/of "abcde") 2 2 "XY")]
    (is (= \a (.charAt g 0)))
    (is (= \b (.charAt g 1)))
    (is (= \X (.charAt g 2)))
    (is (= \Y (.charAt g 3)))
    (is (= \c (.charAt g 4)))
    (is (= \e (.charAt g 6)))))

;; --- subSequence ---

(deftest sub-sequence-before-gap
  ;; gap/of puts gap at end, so all content is before gap
  (let [g (gap/of "hello world")]
    (is (= "hello" (.subSequence g 0 5)))
    (is (= "world" (.subSequence g 6 11)))))

(deftest sub-sequence-after-gap
  ;; After insert at 0, gap is at pos 3 ("XXX" inserted)
  ;; Content: "XXXhello", gap at [3, 3+GAP)
  ;; Positions 3+ are after the gap
  (let [g (gap/edit (gap/of "hello") 0 0 "XXX")]
    (is (= "hello" (.subSequence g 3 8)))))

(deftest sub-sequence-spanning-gap
  ;; Insert at pos 3 of "abcdefgh" → "abcXYdefgh", gap at [5, 5+GAP)
  ;; subSequence(2, 7) = "cXYde" — spans before-gap and after-gap
  (let [g (gap/edit (gap/of "abcdefgh") 3 3 "XY")]
    (is (= "abcXYdefgh" (str g)))
    (is (= "cXYde" (.subSequence g 2 7)))))

(deftest sub-sequence-empty
  (is (= "" (.subSequence (gap/of "hello") 3 3))))

;; --- edit ---

(deftest edit-insert-at-start
  (is (= "Xhello" (str (gap/edit (gap/of "hello") 0 0 "X")))))

(deftest edit-insert-at-end
  (is (= "helloX" (str (gap/edit (gap/of "hello") 5 5 "X")))))

(deftest edit-insert-at-middle
  (is (= "heXllo" (str (gap/edit (gap/of "hello") 2 2 "X")))))

(deftest edit-delete-range
  (is (= "hlo" (str (gap/edit (gap/of "hello") 1 3 "")))))

(deftest edit-replace
  (is (= "hXYo" (str (gap/edit (gap/of "hello") 1 4 "XY")))))

(deftest edit-delete-all
  (is (= "" (str (gap/edit (gap/of "hello") 0 5 "")))))

(deftest edit-on-empty
  (is (= "abc" (str (gap/edit (gap/of "") 0 0 "abc")))))

(deftest edit-sequential-inserts
  ;; Simulate typing "abc" one char at a time
  (let [g0 (gap/of "")
        g1 (gap/edit g0 0 0 "a")
        g2 (gap/edit g1 1 1 "b")
        g3 (gap/edit g2 2 2 "c")]
    (is (= "abc" (str g3)))))

;; --- substr ---

(deftest substr-basic
  (is (= "llo" (gap/substr (gap/of "hello") 2 5))))

(deftest substr-empty-range
  (is (= "" (gap/substr (gap/of "hello") 2 2))))

(deftest substr-spanning-gap
  (let [g (gap/edit (gap/of "abcdefgh") 3 3 "XY")]
    (is (= "cXYde" (gap/substr g 2 7)))))

;; --- equals ---

(deftest equals-same-content
  (is (.equals (gap/of "hello") (gap/of "hello"))))

(deftest equals-different-content
  (is (not (.equals (gap/of "hello") (gap/of "world")))))

(deftest equals-with-string
  ;; GapBuffer.equals(String) compares content
  (is (.equals (gap/of "hello") "hello")))

(deftest equals-different-length
  (is (not (.equals (gap/of "hi") (gap/of "hello")))))

(deftest equals-empty
  (is (.equals (gap/of "") (gap/of ""))))

(deftest equals-after-edit
  (let [g (gap/edit (gap/of "hello") 5 5 " world")]
    (is (.equals g "hello world"))))

(deftest equals-nil
  (is (not (.equals (gap/of "hello") nil))))

;; --- hashCode ---

(deftest hash-code-matches-string
  (is (= (.hashCode "hello") (.hashCode (gap/of "hello"))))
  (is (= (.hashCode "") (.hashCode (gap/of "")))))

(deftest hash-code-consistent-with-equals
  (let [g1 (gap/of "test")
        g2 (gap/edit (gap/of "tes") 3 3 "t")]
    (is (.equals g1 g2))
    (is (= (.hashCode g1) (.hashCode g2)))))

;; --- CharSequence seq interop ---

(deftest seqable
  (is (= [\h \e \l \l \o] (seq (gap/of "hello")))))

(deftest seqable-after-edit
  (is (= [\a \X \b] (seq (gap/edit (gap/of "ab") 1 1 "X")))))
