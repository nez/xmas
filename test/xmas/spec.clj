(ns xmas.spec
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [xmas.gap :as gap]))

;; --- Helpers ---

(defn codepoint-boundaries
  "All valid codepoint boundary positions in a string."
  [^String text]
  (loop [p 0 acc [0]]
    (if (>= p (.length text))
      acc
      (let [np (+ p (Character/charCount (.codePointAt text (int p))))]
        (recur np (conj acc np))))))

;; --- Buffer specs ---

(s/def ::name string?)
(s/def ::text #(instance? CharSequence %))
(s/def ::point nat-int?)
(s/def ::mark (s/nilable nat-int?))
(s/def ::file (s/nilable string?))
(s/def ::modified boolean?)
(s/def ::mode keyword?)
(s/def ::from nat-int?)
(s/def ::old string?)
(s/def ::new string?)
(s/def ::undo-entry (s/keys :req-un [::from ::old ::new]))
(s/def ::undo (s/coll-of ::undo-entry :kind vector?))

(s/def ::buffer
  (s/and
    (s/keys :req-un [::name ::text ::point ::mark ::file ::modified ::mode ::undo])
    #(<= 0 (:point %) (count (:text %)))
    #(or (nil? (:mark %)) (<= 0 (:mark %) (count (:text %))))))

;; --- Editor state specs ---

(s/def ::buf string?)
(s/def ::bufs (s/map-of string? ::buffer))
(s/def ::kill (s/coll-of string? :kind vector?))
(s/def ::msg (s/nilable string?))
(s/def ::scroll nat-int?)
(s/def ::rows pos-int?)
(s/def ::cols pos-int?)
(s/def ::last-key any?)
(s/def ::prompt string?)
(s/def ::on-done fn?)
(s/def ::prev-buf string?)
(s/def ::mini (s/nilable (s/keys :req-un [::prompt ::on-done ::prev-buf])))

(s/def ::editor
  (s/and
    (s/keys :req-un [::buf ::bufs ::kill ::msg ::scroll ::rows ::cols ::last-key])
    #(contains? (:bufs %) (:buf %))))

;; --- Generators ---

(def gen-ascii-text
  "ASCII text with newlines."
  (gen/fmap #(apply str %)
    (gen/vector
      (gen/one-of
        [(gen/fmap char (gen/choose 32 126))
         (gen/return \newline)])
      0 50)))

(def gen-unicode-text
  "Text mixing ASCII, CJK, and newlines."
  (gen/fmap #(apply str %)
    (gen/vector
      (gen/one-of
        [(gen/fmap str (gen/fmap char (gen/choose 32 126)))
         (gen/return "\n")
         (gen/fmap #(String. (Character/toChars %)) (gen/choose 0x4E00 0x4E10))])
      0 30)))

(def gen-text
  (gen/one-of [(gen/return "") gen-ascii-text gen-unicode-text]))

(def gen-buffer
  "Valid buffer with point on a codepoint boundary."
  (gen/let [text gen-text
            idx gen/nat]
    (let [bounds (codepoint-boundaries text)
          point (nth bounds (mod idx (count bounds)))]
      {:name "*test*" :text (gap/of text) :point point :mark nil :file nil
       :modified false :mode :fundamental :undo []})))

(def gen-editor
  "Valid editor state with one buffer."
  (gen/let [buffer gen-buffer
            rows (gen/choose 10 50)
            cols (gen/choose 40 120)]
    {:buf "*test*"
     :bufs {"*test*" buffer}
     :kill [] :msg nil :mini nil
     :scroll 0 :rows rows :cols cols :last-key nil}))

(defn gen-edit-args
  "Generate [buffer from to repl] for valid buf/edit calls."
  []
  (gen/let [buffer gen-buffer
            repl (gen/one-of [(gen/return "") (gen/not-empty gen/string-alphanumeric)])
            i gen/nat
            j gen/nat]
    (let [bounds (codepoint-boundaries (str (:text buffer)))
          n (count bounds)
          p1 (nth bounds (mod i n))
          p2 (nth bounds (mod j n))
          from (min p1 p2)
          to (max p1 p2)]
      [buffer from to repl])))
