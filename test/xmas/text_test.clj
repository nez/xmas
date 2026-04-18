(ns xmas.text-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [xmas.text :as text]
            [xmas.spec :as spec]))

;; --- Properties ---

(defspec next-prev-roundtrip 200
  (prop/for-all [t (gen/such-that #(pos? (count %)) spec/gen-text {:max-tries 50})]
    (let [bounds (butlast (spec/codepoint-boundaries t))]
      (every? #(= % (text/prev-pos t (text/next-pos t %))) bounds))))

(defspec prev-next-roundtrip 200
  (prop/for-all [t (gen/such-that #(pos? (count %)) spec/gen-text)]
    (let [bounds (rest (spec/codepoint-boundaries t))]
      (every? #(= % (text/next-pos t (text/prev-pos t %))) bounds))))

(defspec line-start-valid 200
  (prop/for-all [t (gen/such-that #(pos? (count %)) spec/gen-text)]
    (let [bounds (butlast (spec/codepoint-boundaries t))]
      (every? (fn [p]
        (let [ls (text/line-start t p)]
          (or (zero? ls) (= \newline (.charAt ^String t (dec ls))))))
        bounds))))

(defspec line-end-valid 200
  (prop/for-all [t (gen/such-that #(pos? (count %)) spec/gen-text)]
    (let [bounds (butlast (spec/codepoint-boundaries t))]
      (every? (fn [p]
        (let [le (text/line-end t p)]
          (or (= le (count t)) (= \newline (.charAt ^String t le)))))
        bounds))))

(defspec display-width-non-negative 200
  (prop/for-all [t spec/gen-text]
    (>= (text/display-width t 0 (count t)) 0)))

(defspec pos-at-col-within-bounds 200
  (prop/for-all [t (gen/such-that #(pos? (count %)) spec/gen-text)
                 col (gen/choose 0 100)]
    (let [result (text/pos-at-col t 0 (count t) col)]
      (<= 0 result (count t)))))

;; --- next-pos / prev-pos ---

(deftest next-pos-ascii
  (is (= 1 (text/next-pos "abc" 0)))
  (is (= 2 (text/next-pos "abc" 1)))
  (is (= 3 (text/next-pos "abc" 2))))

(deftest next-pos-at-end
  (is (= 3 (text/next-pos "abc" 3))))

(deftest next-pos-cjk
  (let [t "\u4F60\u597D"]
    (is (= 1 (text/next-pos t 0)))
    (is (= 2 (text/next-pos t 1)))))

(deftest next-pos-emoji
  (let [t "a\uD83D\uDE00b"]  ;; a😀b
    (is (= 1 (text/next-pos t 0)))
    (is (= 3 (text/next-pos t 1)))
    (is (= 4 (text/next-pos t 3)))))

(deftest prev-pos-emoji
  (let [t "a\uD83D\uDE00b"]
    (is (= 3 (text/prev-pos t 4)))
    (is (= 1 (text/prev-pos t 3)))
    (is (= 0 (text/prev-pos t 1)))))

(deftest prev-pos-at-start
  (is (= 0 (text/prev-pos "abc" 0))))

(deftest next-prev-empty
  (is (= 0 (text/next-pos "" 0)))
  (is (= 0 (text/prev-pos "" 0))))

;; --- line-start / line-end ---

(deftest line-start-basic
  (is (= 0 (text/line-start "abc" 0)))
  (is (= 0 (text/line-start "abc" 2)))
  (is (= 4 (text/line-start "abc\ndef" 4)))
  (is (= 4 (text/line-start "abc\ndef" 6))))

(deftest line-end-basic
  (is (= 3 (text/line-end "abc\ndef" 0)))
  (is (= 3 (text/line-end "abc\ndef" 2)))
  (is (= 7 (text/line-end "abc\ndef" 4))))

(deftest line-start-no-newline
  (is (= 0 (text/line-start "abc" 1))))

(deftest line-end-no-newline
  (is (= 3 (text/line-end "abc" 0))))

;; --- char-width ---

(deftest char-width-ascii
  (is (= 1 (text/char-width (int \a))))
  (is (= 1 (text/char-width (int \space)))))

(deftest char-width-cjk
  (is (= 2 (text/char-width 0x4E00)))
  (is (= 2 (text/char-width 0x9FFF))))

(deftest char-width-combining
  (is (= 0 (text/char-width 0x0300)))
  (is (= 0 (text/char-width 0x036F))))

(deftest char-width-fullwidth
  (is (= 2 (text/char-width 0xFF01))))

;; --- display-width ---

(deftest display-width-ascii
  (is (= 5 (text/display-width "hello" 0 5))))

(deftest display-width-cjk
  (let [t "\u4F60\u597D"]  ;; 你好
    (is (= 4 (text/display-width t 0 2)))))

(deftest display-width-mixed
  (let [t "a\u4F60"]  ;; a你
    (is (= 3 (text/display-width t 0 (count t))))))

(deftest display-width-empty
  (is (= 0 (text/display-width "" 0 0))))

;; --- pos-at-col ---

(deftest pos-at-col-ascii
  (is (= 0 (text/pos-at-col "abc" 0 3 0)))
  (is (= 1 (text/pos-at-col "abc" 0 3 1)))
  (is (= 3 (text/pos-at-col "abc" 0 3 3))))

(deftest pos-at-col-cjk-no-split
  (let [t "\u4F60\u597D"]  ;; 你好, each 2 cols
    (is (= 0 (text/pos-at-col t 0 2 0)))
    (is (= 0 (text/pos-at-col t 0 2 1)))  ;; can't split wide char
    (is (= 1 (text/pos-at-col t 0 2 2)))))

(deftest pos-at-col-past-end
  (is (= 3 (text/pos-at-col "abc" 0 3 10))))

;; --- word-forward / word-backward ---

(deftest word-forward-basic
  (is (= 5 (text/word-forward "hello world" 0)))
  (is (= 11 (text/word-forward "hello world" 6))))

(deftest word-forward-at-end
  (is (= 5 (text/word-forward "hello" 5))))

(deftest word-forward-punctuation
  (is (= 5 (text/word-forward "hello, world" 0))))

(deftest word-backward-basic
  (is (= 6 (text/word-backward "hello world" 11)))
  (is (= 0 (text/word-backward "hello world" 5))))

(deftest word-backward-at-start
  (is (= 0 (text/word-backward "hello" 0))))

(deftest word-forward-empty
  (is (= 0 (text/word-forward "" 0))))

(deftest word-backward-empty
  (is (= 0 (text/word-backward "" 0))))

;; --- search-forward ---

(deftest search-forward-basic
  (is (= 6 (text/search-forward "hello world" "world" 0))))

(deftest search-forward-at-start
  (is (= 0 (text/search-forward "hello world" "hello" 0))))

(deftest search-forward-from-middle
  (is (= 6 (text/search-forward "hello world" "world" 3))))

(deftest search-forward-not-found
  (is (nil? (text/search-forward "hello world" "xyz" 0))))

(deftest search-forward-empty-pattern
  (is (nil? (text/search-forward "hello" "" 0))))

(deftest search-forward-past-match
  (is (nil? (text/search-forward "hello world" "hello" 1))))

(deftest search-forward-single-char
  (is (= 4 (text/search-forward "abcde" "e" 0))))

(deftest search-forward-repeated
  ;; finds first occurrence at or after from
  (is (= 0 (text/search-forward "abcabc" "abc" 0)))
  (is (= 3 (text/search-forward "abcabc" "abc" 1))))

;; --- search-backward ---

(deftest search-backward-basic
  (is (= 6 (text/search-backward "hello world" "world" 11))))

(deftest search-backward-at-start
  (is (= 0 (text/search-backward "hello world" "hello" 11))))

(deftest search-backward-from-middle
  (is (= 0 (text/search-backward "hello world" "hello" 6))))

(deftest search-backward-not-found
  (is (nil? (text/search-backward "hello world" "xyz" 11))))

(deftest search-backward-empty-pattern
  (is (nil? (text/search-backward "hello" "" 5))))

(deftest search-backward-before-match
  ;; "world" starts at 6, searching before pos 6 shouldn't find it
  (is (nil? (text/search-backward "hello world" "world" 6))))

(deftest search-backward-repeated
  (is (= 3 (text/search-backward "abcabc" "abc" 6)))
  (is (= 0 (text/search-backward "abcabc" "abc" 3))))

(deftest search-backward-from-zero-is-nil
  ;; Regression: nothing can be strictly before position 0.
  (is (nil? (text/search-backward "abc" "a" 0))))

(deftest search-backward-pattern-longer-than-text
  ;; Regression: pattern longer than the text used to trigger IOOBE because
  ;; `start` clamped to 0 and match-at? read past the end.
  (is (nil? (text/search-backward "ab" "abcde" 2)))
  (is (nil? (text/search-backward "x" "xy" 1))))

(deftest line-start-clamps-pos-past-end
  ;; Regression: line-start with pos > length used to walk off the end and
  ;; crash with StringIndexOutOfBoundsException.
  (is (= 0 (text/line-start "abc" 100)))
  (is (= 4 (text/line-start "abc\nxyz" 999))))

(deftest search-backward-rejects-straddling-match
  ;; Regression: search-backward used to return a match whose end was
  ;; past `from`. "bb" in "abbab" from=2 would return 1, but match ends
  ;; at 3 > 2. The match must be fully before `from`.
  (is (nil? (text/search-backward "abbab" "bb" 2)))
  (is (= 1 (text/search-backward "abbab" "bb" 5))))
