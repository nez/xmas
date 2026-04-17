(ns xmas.term
  (:require [clojure.string :as str]
            [xmas.log :as log])
  (:import [java.io InputStream]))

;; Raw terminal via stty + System.in/out. No JLine.

(defn stty! [& args]
  (let [cmd (into-array String ["sh" "-c" (str "stty " (str/join " " args) " </dev/tty")])
        p (.exec (Runtime/getRuntime) cmd)]
    (.waitFor p)))

(defn terminal-size []
  (let [p (.exec (Runtime/getRuntime) (into-array String ["sh" "-c" "stty size </dev/tty"]))
        out (slurp (.getInputStream p))
        [r c] (str/split (str/trim out) #"\s+")]
    [(Integer/parseInt r) (Integer/parseInt c)]))

(defn enter-raw-mode! [] (stty! "raw" "-echo" "-icanon" "-isig"))
(defn exit-raw-mode! [] (stty! "cooked" "echo" "icanon" "isig"))

(defn on-resize!
  "Register a callback (fn [rows cols]) for terminal resize (SIGWINCH)."
  [callback]
  (let [handler (proxy [sun.misc.SignalHandler] []
                  (handle [_sig]
                    (try
                      (let [[rows cols] (terminal-size)]
                        (callback rows cols))
                      (catch Exception e
                        (log/log "resize error:" (.getMessage e))))))]
    (sun.misc.Signal/handle (sun.misc.Signal. "WINCH") handler)))

;; --- Input from System.in ---

(def ^InputStream stdin System/in)

(defn byte-available? [] (> (.available stdin) 0))

(defn read-byte-timeout ^long [^long ms]
  (let [deadline (+ (System/currentTimeMillis) ms)]
    (loop []
      (cond
        (byte-available?) (let [b (.read stdin)] (log/log "byte-t:" b) b)
        (>= (System/currentTimeMillis) deadline) -1
        :else (let [remaining (- deadline (System/currentTimeMillis))]
                (Thread/sleep (min 10 (max 1 remaining)))
                (recur))))))

(defn- read-utf8
  "Decode a multi-byte UTF-8 sequence given the leading byte."
  [^long b]
  (let [[n mask] (cond (< b 0xC0) [0 0]
                       (< b 0xE0) [1 0x1F]
                       (< b 0xF0) [2 0x0F]
                       :else      [3 0x07])]
    (when (pos? n)
      (let [cp (loop [i 0 cp (bit-and b mask)]
                 (if (>= i n) cp
                   (let [c (read-byte-timeout 50)]
                     (if (or (< c 0x80) (>= c 0xC0))
                       -1
                       (recur (inc i) (bit-or (bit-shift-left cp 6)
                                              (bit-and c 0x3F)))))))]
        (when (pos? cp)
          (if (<= cp 0xFFFF) (char cp)
            (String. (Character/toChars cp))))))))

(defn- parse-csi []
  (loop [params ""]
    (let [b (read-byte-timeout 50)]
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
      (= b 79)  (let [b2 (read-byte-timeout 50)]
                  (if (< b2 0) nil
                    (case b2 80 :f1 81 :f2 82 :f3 83 :f4 nil)))
      (>= b 32) [:meta (char b)]
      (< b 27)  [:meta [:ctrl (char (+ b 96))]]
      :else nil)))

(defn- byte->key
  "Map a leading byte to a key value."
  [^long b]
  (cond
    (< b 0)   nil
    (= b 27)  (parse-escape)
    (= b 9)   :tab
    (= b 13)  :return
    (= b 10)  :return
    (= b 127) :backspace
    (< b 27)  [:ctrl (char (+ b 96))]
    (< b 32)  nil
    (< b 128) (char b)
    :else     (read-utf8 b)))

(defn read-key-timeout
  "Like read-key but returns nil after ms instead of blocking forever."
  [ms]
  (let [b (read-byte-timeout ms)]
    (when (>= b 0)
      (let [key (byte->key b)]
        (log/log "key-t:" (pr-str key))
        key))))

;; --- Output to System.out ---

(def ^:private E "\033[")
(def ^java.io.PrintStream real-out System/out)
(def ^:dynamic *out-fn* (fn [^String s] (.print real-out s)))

(defn tw [s] (*out-fn* (str s)))
(defn flush! [] (.flush real-out))

(defmacro with-frame
  "Batch all tw output into a single write+flush."
  [& body]
  `(let [sb# (StringBuilder. 4096)
         outer# *out-fn*
         result# (binding [*out-fn* (fn [s#] (.append sb# s#))]
                   ~@body)]
     (outer# (.toString sb#))
     (.flush real-out)
     result#))

(defn- esc [& xs] (tw (apply str E xs)))

(defn move [r c] (esc (inc r) ";" (inc c) "H"))
(defn clreol   [] (esc "K"))
(defn cls      [] (esc "2J" E "H"))
(defn show-cur [] (esc "?25h"))
(defn hide-cur [] (esc "?25l"))
(defn reset-sg [] (esc "0m"))

(defn sg [{:keys [fg bg bold]}]
  (esc (str/join ";" (cond-> [0] bold (conj 1) fg (conj 38 5 fg) bg (conj 48 5 bg))) "m"))

(defn teardown!
  "Restore the terminal to a sane state. Each step is isolated so a failure
   in one doesn't abort the rest."
  []
  (doseq [f [reset-sg cls #(move 0 0) show-cur flush! exit-raw-mode!]]
    (try (f) (catch Exception _))))

(defn install-shutdown-hook!
  "Safety net: restore terminal even on unexpected JVM exit."
  []
  (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable teardown!)))
