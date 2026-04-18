(ns xmas.overlay-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.overlay :as ov]))

(deftest make-with-defaults
  (let [o (ov/make 5 10)]
    (is (= 5 (:from o)))
    (is (= 10 (:to o)))
    (is (= :default (:face o)))))

(deftest adjust-before-edit-unchanged
  (let [ovs [(ov/make 0 5)]
        ovs' (ov/adjust ovs 10 15 0)]
    (is (= [0 5] [(:from (first ovs')) (:to (first ovs'))]))))

(deftest adjust-after-edit-shifts-by-delta
  ;; edit at [10 15), replaced with 2 chars → delta = -3
  (let [ovs [(ov/make 20 30)]
        ovs' (ov/adjust ovs 10 15 2)]
    (is (= [17 27] [(:from (first ovs')) (:to (first ovs'))]))))

(deftest adjust-edit-contains-overlay-collapses
  (let [ovs [(ov/make 5 8)]
        ovs' (ov/adjust ovs 0 10 0)]
    ;; overlay was fully inside edit → collapsed to zero-length → dropped
    (is (empty? ovs'))))

(deftest adjust-edit-overlaps-start-trims
  (let [ovs [(ov/make 5 15)]
        ovs' (ov/adjust ovs 0 10 0)]
    ;; edit [0,10) deleted; overlay now starts at 0 and ends at 5
    (is (= [0 5] [(:from (first ovs')) (:to (first ovs'))]))))

(deftest adjust-edit-overlaps-end-trims
  (let [ovs [(ov/make 5 15)]
        ovs' (ov/adjust ovs 10 20 0)]
    (is (= [5 10] [(:from (first ovs')) (:to (first ovs'))]))))

(deftest adjust-edit-inside-overlay-stretches
  (let [ovs [(ov/make 5 15)]
        ovs' (ov/adjust ovs 8 10 5)]
    ;; 2 chars replaced with 5 → +3 to the tail
    (is (= [5 18] [(:from (first ovs')) (:to (first ovs'))]))))

(deftest in-range-filters
  (let [ovs [(ov/make 0 5)
             (ov/make 10 15)
             (ov/make 20 25)]]
    (is (= [10 15] (let [o (first (ov/in-range ovs 12 14))]
                     [(:from o) (:to o)])))))

(deftest adjust-preserves-intentionally-empty-overlays
  ;; Regression: `adjust` unconditionally dropped overlays with :from == :to,
  ;; so empty "cursor marker" overlays disappeared after any unrelated edit.
  (let [ovs  [(ov/make 5 5 :cursor)]
        ovs' (ov/adjust ovs 20 22 3)]
    (is (= 1 (count ovs')))
    (is (= [5 5] [(:from (first ovs')) (:to (first ovs'))]))))
