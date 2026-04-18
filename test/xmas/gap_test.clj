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

;; --- Line index ---

(deftest line-count-no-newlines
  (is (= 1 (gap/line-count (gap/of "hello")))))

(deftest line-count-empty
  (is (= 1 (gap/line-count (gap/of "")))))

(deftest line-count-multi
  (is (= 3 (gap/line-count (gap/of "abc\ndef\nghi")))))

(deftest line-count-trailing-newline
  (is (= 4 (gap/line-count (gap/of "a\nb\nc\n")))))

(deftest line-of-single-line
  (let [g (gap/of "hello")]
    (is (= 0 (gap/line-of g 0)))
    (is (= 0 (gap/line-of g 3)))
    (is (= 0 (gap/line-of g 5)))))

(deftest line-of-multi
  (let [g (gap/of "abc\ndef\nghi")]
    (is (= 0 (gap/line-of g 0)))   ;; 'a'
    (is (= 0 (gap/line-of g 3)))   ;; '\n'
    (is (= 1 (gap/line-of g 4)))   ;; 'd'
    (is (= 1 (gap/line-of g 7)))   ;; '\n'
    (is (= 2 (gap/line-of g 8)))   ;; 'g'
    (is (= 2 (gap/line-of g 11))))) ;; end

(deftest line-of-empty
  (is (= 0 (gap/line-of (gap/of "") 0))))

(deftest nth-line-start-basic
  (let [g (gap/of "abc\ndef\nghi")]
    (is (= 0 (gap/nth-line-start g 0)))
    (is (= 4 (gap/nth-line-start g 1)))
    (is (= 8 (gap/nth-line-start g 2)))))

(deftest nth-line-end-basic
  (let [g (gap/of "abc\ndef\nghi")]
    (is (= 3 (gap/nth-line-end g 0)))
    (is (= 7 (gap/nth-line-end g 1)))
    (is (= 11 (gap/nth-line-end g 2)))))  ;; last line, no trailing \n

(deftest nth-line-end-trailing-newline
  (let [g (gap/of "abc\n")]
    (is (= 3 (gap/nth-line-end g 0)))     ;; \n position
    (is (= 4 (gap/nth-line-end g 1)))))   ;; empty last line, end = text length

(deftest line-index-after-edit
  ;; Insert a newline, verify the index updates
  (let [g (gap/edit (gap/of "abcdef") 3 3 "\n")]
    (is (= "abc\ndef" (str g)))
    (is (= 2 (gap/line-count g)))
    (is (= 0 (gap/line-of g 0)))
    (is (= 1 (gap/line-of g 4)))
    (is (= 0 (gap/nth-line-start g 0)))
    (is (= 4 (gap/nth-line-start g 1)))
    (is (= 3 (gap/nth-line-end g 0)))
    (is (= 7 (gap/nth-line-end g 1)))))

(deftest line-index-after-delete-newline
  ;; Delete a newline, verify lines merge
  (let [g (gap/edit (gap/of "abc\ndef") 3 4 "")]
    (is (= "abcdef" (str g)))
    (is (= 1 (gap/line-count g)))
    (is (= 0 (gap/line-of g 3)))))

(deftest incremental-lidx-matches-rescan
  ;; The incremental splice inside gap/edit must produce the same newline
  ;; index as building a fresh buffer from the resulting string.
  (letfn [(nls [g] (vec (for [i (range (count g))
                              :when (= \newline (.charAt g (int i)))] i)))]
    (doseq [[init edits] [["abc\ndef\nghi" [[0 0 "X"] [3 4 ""] [0 3 "\n\n"]]]
                          ["" [[0 0 "a\nb\nc"] [2 3 "XX\nY"] [0 5 ""]]]
                          ["one\ntwo\nthree\nfour"
                           [[4 7 "TWO"] [0 3 "ONE"] [8 13 ""] [0 0 "\n\n\n"]]]
                          ["line\n" [[4 5 ""] [0 0 "\n"] [5 5 "\n\n"]]]]]
      (loop [g (gap/of init) es edits]
        (if-let [[f t r] (first es)]
          (let [g' (gap/edit g f t r)]
            (is (= (nls g') (nls (gap/of (str g'))))
                (str "mismatch after " (pr-str [f t r]) " on " (pr-str (str g))))
            (recur g' (rest es)))
          :done)))))
