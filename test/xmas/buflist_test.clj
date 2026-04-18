(ns xmas.buflist-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.buflist :as buflist]
            [xmas.cmd :as cmd]
            [xmas.gap :as gap]))

(defn- state-with-bufs [bufs cur]
  {:buf cur :bufs bufs :msg nil})

(deftest open-creates-read-only-buffer
  (let [s0 (state-with-bufs {"a" (buf/make "a" "hi" nil)
                             "b" (buf/make "b" "bye" nil)} "a")
        s (buflist/open s0)]
    (is (= "*Buffer List*" (:buf s)))
    (is (true? (:read-only (cmd/cur s))))
    (is (.contains ^String (str (:text (cmd/cur s))) "a"))
    (is (.contains ^String (str (:text (cmd/cur s))) "b"))))

(deftest open-lists-visible-buffers
  (let [s0 (state-with-bufs {"a" (buf/make "a" "" nil)
                             " *mini*" (buf/make " *mini*" "" nil)} "a")
        s (buflist/open s0)
        entries (:buflist-entries (cmd/cur s))]
    (is (= ["a"] (map :name entries)))))

(deftest switch-moves-to-buffer-at-point
  (let [s0 (state-with-bufs {"a" (buf/make "a" "" nil)
                             "b" (buf/make "b" "" nil)} "a")
        s (buflist/open s0)
        ;; move point to line 1 (first entry)
        s (cmd/update-cur s #(assoc % :point (gap/nth-line-start (:text %) 1)))
        s' (buflist/switch s)]
    (is (= "a" (:buf s')))))

(deftest kill-removes-buffer
  (let [s0 (state-with-bufs {"a" (buf/make "a" "" nil)
                             "b" (buf/make "b" "" nil)} "a")
        s (buflist/open s0)
        ;; move to line 2 = "b"
        s (cmd/update-cur s #(assoc % :point (gap/nth-line-start (:text %) 2)))
        s' (buflist/kill s)]
    (is (not (contains? (:bufs s') "b")))
    (is (.contains ^String (str (:msg s')) "Killed b"))))

(deftest kill-on-header-is-noop
  (let [s0 (state-with-bufs {"a" (buf/make "a" "" nil)} "a")
        s (buflist/open s0)
        ;; point at line 0 = header
        s' (buflist/kill s)]
    (is (contains? (:bufs s') "a"))))

(deftest kill-preserves-point
  ;; Regression: buflist/kill used to rebuild the *Buffer List* buffer
  ;; with point=0, so the cursor jumped to the header after pressing `k`.
  (let [s0 (state-with-bufs {"a" (buf/make "a" "" nil)
                             "b" (buf/make "b" "" nil)
                             "c" (buf/make "c" "" nil)} "a")
        s  (buflist/open s0)
        ;; move to line 2 = "b"
        s  (cmd/update-cur s #(assoc % :point (gap/nth-line-start (:text %) 2)))
        p-before (:point (cmd/cur s))
        s' (buflist/kill s)]
    (is (= p-before (:point (cmd/cur s'))))))
