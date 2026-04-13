(ns xmas.term
  (:require [xmas.log :as log])
  (:import [java.io InputStream]))

;; Raw terminal via stty + System.in/out. No JLine.

(defn stty! [& args]
  (let [cmd (into-array String ["sh" "-c" (str "stty " (clojure.string/join " " args) " </dev/tty")])
        p (.exec (Runtime/getRuntime) cmd)]
    (.waitFor p)))

(defn terminal-size []
  (let [p (.exec (Runtime/getRuntime) (into-array String ["sh" "-c" "stty size </dev/tty"]))
        out (slurp (.getInputStream p))
        [r c] (clojure.string/split (clojure.string/trim out) #"\s+")]
    [(Integer/parseInt r) (Integer/parseInt c)]))

(defn enter-raw-mode! [] (stty! "raw" "-echo" "-icanon" "-isig"))
(defn exit-raw-mode! [] (stty! "cooked" "echo" "icanon" "isig"))

;; --- Input from System.in ---

(def ^InputStream stdin System/in)

(defn read-byte-raw ^long []
  (let [b (.read stdin)]
    (log/log "byte:" b)
    b))

(defn byte-available? [] (> (.available stdin) 0))

(defn read-byte-timeout ^long [^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (byte-available?) (let [b (.read stdin)] (log/log "byte-t:" b) b)
        (>= (System/currentTimeMillis) deadline) -1
        :else (do (Thread/sleep 1) (recur))))))

(defn- parse-csi []
  (loop [params ""]
    (let [b (read-byte-raw)]
      (cond
        (< b 0) nil
        (<= 64 b 126)
        (case (char b)
          \A :up, \B :down, \C :right, \D :left, \H :home, \F :end
          \~ (case params "2" :insert "3" :delete "5" :page-up "6" :page-down nil)
          nil)
        :else (recur (str params (char b)))))))

(defn- parse-escape []
  (let [b (read-byte-timeout 50)]
    (cond
      (< b 0)   :escape
      (= b 91)  (parse-csi)
      (= b 79)  (let [b2 (read-byte-raw)]
                  (case b2 80 :f1 81 :f2 82 :f3 83 :f4 nil))
      (>= b 32) [:meta (char b)]
      (< b 27)  [:meta [:ctrl (char (+ b 96))]]
      :else nil)))

(defn read-key []
  (let [b (read-byte-raw)
        key (cond
              (< b 0)  nil
              (= b 27) (parse-escape)
              (= b 9)  :tab
              (= b 13) :return
              (= b 10) :return
              (= b 127) :backspace
              (< b 27) [:ctrl (char (+ b 96))]
              (< b 32) nil
              :else    (char b))]
    (log/log "key:" (pr-str key))
    key))

;; --- Output to System.out ---

(def ^:private E "\033[")
(def ^java.io.PrintStream out System/out)

(defn tw [^String s] (.print out s))
(defn flush! [] (.flush out))
(defn move [r c] (tw (str E (inc r) ";" (inc c) "H")))
(defn clreol [] (tw (str E "K")))
(defn cls [] (tw (str E "2J" E "H")))
(defn show-cur [] (tw (str E "?25h")))
(defn hide-cur [] (tw (str E "?25l")))
(defn reset-sg [] (tw (str E "0m")))

(defn sg [{:keys [fg bg bold]}]
  (let [c (cond-> [0] bold (conj 1) fg (conj 38 5 fg) bg (conj 48 5 bg))]
    (tw (str E (clojure.string/join ";" c) "m"))))
