(ns xmas.window-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.window :as w]))

(deftest leaf-has-defaults
  (is (= {:type :leaf :buffer "a" :scroll 0 :hscroll 0} (w/leaf "a"))))

(deftest split-produces-two-leaves
  (let [[t p] (w/split (w/leaf "a") [] :stacked)]
    (is (= [:a] p))
    (is (= :split (:type t)))
    (is (= "a" (get-in t [:a :buffer])))
    (is (= "a" (get-in t [:b :buffer])))))

(deftest leaves-pre-order
  (let [t (-> (w/leaf "a") (w/split [] :stacked) first
              (w/split [:b] :side-by-side) first)]
    ;; tree: stacked(leaf a, side-by-side(leaf a, leaf a))
    (is (= [[:a] [:b :a] [:b :b]] (w/leaves t)))))

(deftest next-leaf-cycles
  (let [t (-> (w/leaf "a") (w/split [] :stacked) first)
        all (w/leaves t)]
    (is (= [:b] (w/next-leaf t [:a])))
    (is (= [:a] (w/next-leaf t [:b])))))

(deftest prev-leaf-cycles
  (let [t (-> (w/leaf "a") (w/split [] :stacked) first)]
    (is (= [:b] (w/prev-leaf t [:a])))
    (is (= [:a] (w/prev-leaf t [:b])))))

(deftest delete-window-promotes-sibling
  (let [[t p] (w/split (w/leaf "a") [] :stacked)
        [t2 p2] (w/delete-window t [:a])]
    (is (w/leaf? t2))
    (is (= [] p2))))

(deftest delete-window-in-nested-tree
  (let [t (-> (w/leaf "a") (w/split [] :stacked) first
              (w/split [:b] :side-by-side) first)
        [t2 p2] (w/delete-window t [:b :a])]
    ;; killing :b :a promotes :b :b up into :b
    (is (= "a" (get-in t2 [:b :buffer])))
    (is (= [:b] p2))))

(deftest delete-only-window-is-noop
  (let [t (w/leaf "a")
        [t2 p2] (w/delete-window t [])]
    (is (= t t2)) (is (= [] p2))))

(deftest only-window-collapses
  (let [t (-> (w/leaf "a") (w/split [] :stacked) first)
        [t2 p2] (w/only t [:a])]
    (is (w/leaf? t2)) (is (= [] p2))))

(deftest layout-single-leaf
  (is (= {[] {:row 0 :col 0 :rows 24 :cols 80}}
         (w/layout (w/leaf "a") 24 80))))

(deftest layout-stacked-split
  (let [t (first (w/split (w/leaf "a") [] :stacked))
        l (w/layout t 24 80)]
    ;; divider takes 1 row; top = 12, bottom = 11
    (is (= {:row 0 :col 0 :rows 12 :cols 80} (get l [:a])))
    (is (= {:row 13 :col 0 :rows 11 :cols 80} (get l [:b])))))

(deftest layout-side-by-side-split
  (let [t (first (w/split (w/leaf "a") [] :side-by-side))
        l (w/layout t 24 80)]
    (is (= {:row 0 :col 0 :rows 24 :cols 40} (get l [:a])))
    (is (= {:row 0 :col 41 :rows 24 :cols 39} (get l [:b])))))

(deftest dividers-for-stacked
  (let [t (first (w/split (w/leaf "a") [] :stacked))
        ds (w/dividers t 24 80)]
    (is (= [{:kind :horizontal :row 12 :col 0 :len 80}] ds))))

(deftest dividers-for-side-by-side
  (let [t (first (w/split (w/leaf "a") [] :side-by-side))
        ds (w/dividers t 24 80)]
    (is (= [{:kind :vertical :row 0 :col 40 :len 24}] ds))))

;; --- Resize ---

(deftest split-carries-default-ratio
  (is (= 0.5 (:ratio (first (w/split (w/leaf "a") [] :stacked))))))

(deftest adjust-size-enlarges-a-side
  (let [t (first (w/split (w/leaf "a") [] :side-by-side))
        t' (w/adjust-size t [:a] :side-by-side 80 4)]
    (is (< 0.5 (:ratio t')))))

(deftest adjust-size-enlarging-b-shrinks-ratio
  (let [t (first (w/split (w/leaf "a") [] :side-by-side))
        t' (w/adjust-size t [:b] :side-by-side 80 4)]
    (is (> 0.5 (:ratio t')))))

(deftest adjust-size-no-matching-ancestor-is-noop
  (let [t (first (w/split (w/leaf "a") [] :stacked))
        t' (w/adjust-size t [:a] :side-by-side 80 4)]
    (is (= t t'))))

(deftest adjust-size-respects-bounds
  (let [t (first (w/split (w/leaf "a") [] :side-by-side))
        ;; huge delta is clamped to max-ratio 0.9
        t' (w/adjust-size t [:a] :side-by-side 10 1000)]
    (is (>= 0.9 (:ratio t')))
    (is (< 0.5 (:ratio t')))))

(deftest layout-uses-ratio
  (let [t (-> (first (w/split (w/leaf "a") [] :side-by-side))
              (assoc :ratio 0.75))
        l (w/layout t 24 80)]
    ;; 75% of 80 = 60 → a.cols=60, b.cols=80-60-1=19
    (is (= 60 (:cols (get l [:a]))))
    (is (= 19 (:cols (get l [:b]))))))
