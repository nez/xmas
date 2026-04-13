(ns xmas.web-test
  (:require [clojure.test :refer [deftest is testing]]
            [xmas.web :as web]
            [xmas.buf :as buf]
            [xmas.ed :as ed]))

(defn make-state [text]
  {:buf "*test*"
   :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point 0)}
   :kill [] :msg nil :mini nil
   :scroll 0 :rows 24 :cols 80 :last-key nil})

(deftest render-to-string-produces-output
  (let [s (make-state "hello\nworld")
        result (web/render-to-string s)]
    (is (string? result))
    (is (pos? (count result)))
    (is (.contains result "hello"))))

(deftest render-to-string-contains-mode-line
  (let [s (make-state "test")
        result (web/render-to-string s)]
    (is (.contains result "*test*"))))

(deftest render-to-string-does-not-corrupt-term-out
  (let [s (make-state "abc")
        ;; capture that real term/out is unaffected
        before (str xmas.term/real-out)]
    (web/render-to-string s)
    (is (= before (str xmas.term/real-out)))))

(deftest html-resource-exists
  (is (some? (clojure.java.io/resource "xmas/client.html"))))
