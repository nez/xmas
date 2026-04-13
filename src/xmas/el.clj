(ns xmas.el
  (:refer-clojure :exclude [read])
  (:import [java.io PushbackReader StringReader]))

;; --- Helpers ---

(defn- read-ch
  "Read one char from rdr, or nil on EOF."
  [^PushbackReader rdr]
  (let [c (.read rdr)]
    (when (>= c 0) (char c))))

(defn- unread-ch [^PushbackReader rdr ch]
  (.unread rdr (int ch)))

(defn- peek-ch [^PushbackReader rdr]
  (when-let [ch (read-ch rdr)]
    (unread-ch rdr ch)
    ch))

(defn- err [msg]
  (throw (ex-info msg {:detail msg})))

(defn- ws? [ch]
  (or (= ch \space) (= ch \tab) (= ch \newline) (= ch \return)))

(defn- skip-ws [^PushbackReader rdr]
  (loop []
    (let [ch (read-ch rdr)]
      (cond
        (nil? ch)   nil
        (ws? ch)    (recur)
        (= ch \;)   (do (loop [] (let [c (read-ch rdr)]
                                   (when (and c (not= c \newline)) (recur))))
                        (recur))
        :else       (unread-ch rdr ch)))))

;; --- Token delimiters ---

(def ^:private delimiters #{\( \) \[ \] \" \' \; })

(defn- delimiter? [ch]
  (or (nil? ch) (ws? ch) (contains? delimiters ch)))

;; --- Type parsers ---

(defn- read-string-literal [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (nil? ch)  (err "Unterminated string literal")
          (= ch \")  (.toString sb)
          (= ch \\)  (let [esc (read-ch rdr)]
                       (case esc
                         \\ (.append sb \\)
                         \" (.append sb \")
                         \n (.append sb \newline)
                         \t (.append sb \tab)
                         (if (nil? esc)
                           (err "Unterminated string escape")
                           (err (str "Unknown string escape: \\" esc))))
                       (recur))
          :else      (do (.append sb ch) (recur)))))))

(declare read)

(defn- read-list [^PushbackReader rdr]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch)  (err "Unterminated list")
        (= ch \))  (do (read-ch rdr) (apply list acc))
        :else      (recur (conj acc (read rdr)))))))

(defn- read-vector [^PushbackReader rdr]
  (loop [acc []]
    (skip-ws rdr)
    (let [ch (peek-ch rdr)]
      (cond
        (nil? ch)  (err "Unterminated vector")
        (= ch \])  (do (read-ch rdr) acc)
        :else      (recur (conj acc (read rdr)))))))

(defn- read-quote [^PushbackReader rdr]
  (let [form (read rdr)]
    (when (= form ::eof) (err "Unexpected EOF after quote"))
    (list 'quote form)))

(defn- read-char-literal [^PushbackReader rdr]
  (let [ch (read-ch rdr)]
    (cond
      (nil? ch)  (err "Unexpected EOF after ?")
      (= ch \\)  (let [esc (read-ch rdr)]
                   (case esc
                     \n \newline
                     \t \tab
                     \s \space
                     \\ \\
                     (if (nil? esc)
                       (err "Unexpected EOF in character literal")
                       (err (str "Unknown character escape: \\" esc)))))
      :else      ch)))

(defn- parse-token [^String tok]
  (cond
    (= tok "nil") nil
    :else
    (or (try (Long/parseLong tok) (catch NumberFormatException _ nil))
        (try (Double/parseDouble tok) (catch NumberFormatException _ nil))
        (symbol tok))))

(defn- read-token [^PushbackReader rdr]
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (read-ch rdr)]
        (cond
          (delimiter? ch) (do (when ch (unread-ch rdr ch))
                              (let [tok (.toString sb)]
                                (when (empty? tok) (err "Unexpected delimiter"))
                                (parse-token tok)))
          :else           (do (.append sb ch) (recur)))))))

;; --- Public API ---

(defn read
  "Read one elisp form from a PushbackReader.
   Returns ::eof on end of input."
  [^PushbackReader rdr]
  (skip-ws rdr)
  (let [ch (read-ch rdr)]
    (cond
      (nil? ch) ::eof
      (= ch \") (read-string-literal rdr)
      (= ch \() (read-list rdr)
      (= ch \)) (err "Unexpected ')'")
      (= ch \[) (read-vector rdr)
      (= ch \]) (err "Unexpected ']'")
      (= ch \') (read-quote rdr)
      (= ch \?) (read-char-literal rdr)
      :else     (do (unread-ch rdr ch) (read-token rdr)))))

(defn read-all
  "Read all elisp forms from a string. Returns a vector."
  [^String s]
  (let [rdr (PushbackReader. (StringReader. s))]
    (loop [forms []]
      (let [form (read rdr)]
        (if (= form ::eof)
          forms
          (recur (conj forms form)))))))
