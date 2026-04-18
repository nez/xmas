(ns xmas.view-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.view :as view]
            [xmas.window :as win]))

;; --- merge-region: overlay [r-lo r-hi) onto pre-existing face spans ---

(deftest merge-region-no-region
  (is (= [[0 5 :default]] (view/merge-region [[0 5 :default]] nil nil))))

(deftest merge-region-empty-region
  (is (= [[0 5 :default]] (view/merge-region [[0 5 :default]] 3 3))))

(deftest merge-region-disjoint-before
  (is (= [[0 5 :default]] (view/merge-region [[0 5 :default]] 10 15))))

(deftest merge-region-disjoint-after
  ;; spans start at 10, region 0..5 → no overlap
  (is (= [[10 15 :default]] (view/merge-region [[10 15 :default]] 0 5))))

(deftest merge-region-fully-inside
  (is (= [[0 2 :default] [2 5 :region] [5 10 :default]]
         (view/merge-region [[0 10 :default]] 2 5))))

(deftest merge-region-covers-span
  (is (= [[2 8 :region]] (view/merge-region [[2 8 :default]] 0 10))))

(deftest merge-region-splits-multiple-spans
  ;; Two colored spans, region overlays both
  (is (= [[0 2 :builtin] [2 5 :region] [5 7 :region] [7 10 :default]]
         (view/merge-region [[0 5 :builtin] [5 10 :default]] 2 7))))

(deftest merge-region-starts-mid-span
  (is (= [[0 2 :default] [2 5 :region]]
         (view/merge-region [[0 5 :default]] 2 10))))

(deftest merge-region-ends-mid-span
  (is (= [[0 3 :region] [3 5 :default]]
         (view/merge-region [[0 5 :default]] 0 3))))

;; --- apply-face-range directly ---

(deftest apply-face-range-general-face
  (is (= [[0 2 :default] [2 5 :builtin] [5 10 :default]]
         (view/apply-face-range [[0 10 :default]] 2 5 :builtin))))

;; --- render with a 0-body-row pane must not crash ---

(deftest render-tiny-split-doesnt-crash
  ;; Regression: a stacked split of a 3-row frame leaves the pane with a
  ;; 1-row rect, i.e. body-rows = 0. ensure-visible used to unconditionally
  ;; compute nth-line-start of (inc point-line) in that branch, which
  ;; blew up with AIOOB for a point on the last line.
  (let [b (assoc (buf/make "t" "a\nb\nc\nd\ne" nil) :point 8)
        [tree path] (win/split (win/leaf "t") [] :stacked)
        s {:buf "t" :bufs {"t" b}
           :windows tree :cur-window path
           :rows 3 :cols 10}]
    (is (map? (view/render s)))))
