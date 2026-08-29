(ns xmas.web-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [org.httpkit.server :as http]
            [xmas.buf :as buf]
            [xmas.term :as term]
            [xmas.web :as web]
            [xmas.window :as win]))

(defn make-state [text]
  {:buf "*test*"
   :bufs {"*test*" (assoc (buf/make "*test*" text nil) :point 0)}
   :kill [] :msg nil :mini nil
   :windows (win/leaf "*test*") :cur-window []
   :rows 24 :cols 80})

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
        before (str term/real-out)]
    (web/render-to-string s)
    (is (= before (str term/real-out)))))

(deftest render-sanitizes-buffer-control-bytes
  (let [payload (str (char 27) "[31mPWN")
        result (web/render-to-string (make-state payload))]
    (is (not (.contains result payload)))
    (is (.contains result "�[31mPWN"))))

(deftest websocket-keys-are-bounded-and-validated
  (is (= [:ctrl \x] (#'web/parse-key "[:ctrl \\x]")))
  (is (= "😀" (#'web/parse-key "\"😀\"")))
  (is (nil? (#'web/parse-key "{:not :a-key}")))
  (is (nil? (#'web/parse-key (apply str (repeat 65 "x"))))))

(deftest server-binds-to-loopback
  (let [opts (atom nil)
        editor (atom (make-state ""))]
    (with-redefs [http/run-server (fn [_handler options]
                                    (reset! opts options)
                                    (fn []))]
      (let [srv (web/start! editor 1234 identity)]
        (is (= "127.0.0.1" (:ip @opts)))
        (web/stop! srv editor)))))

(deftest html-resource-exists
  (is (some? (io/resource "xmas/client.html"))))
