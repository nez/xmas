(ns xmas.text)

(defn next-pos
  "Advance past one codepoint."
  ^long [^CharSequence t ^long p]
  (if (>= p (.length t)) p
    (+ p (Character/charCount (Character/codePointAt t (int p))))))

(defn prev-pos
  "Retreat past one codepoint."
  ^long [^CharSequence t ^long p]
  (if (<= p 0) 0
    (- p (Character/charCount (Character/codePointBefore t (int p))))))

(defn line-start
  "Start of line containing pos."
  ^long [^CharSequence t ^long pos]
  (if (<= pos 0) 0
    (loop [i (dec pos)]
      (cond
        (neg? i) 0
        (= (.charAt t (int i)) \newline) (inc i)
        :else (recur (dec i))))))

(defn line-end
  "End of line containing pos (position of newline, or end of text)."
  ^long [^CharSequence t ^long pos]
  (let [n (.length t)]
    (loop [i pos]
      (cond
        (>= i n) n
        (= (.charAt t (int i)) \newline) i
        :else (recur (inc i))))))

(defn char-width
  "Terminal display width of a Unicode codepoint."
  ^long [^long cp]
  (cond
    (<= 0x0300 cp 0x036F) 0   ;; combining diacriticals
    (<= 0x1AB0 cp 0x1AFF) 0   ;; combining extended
    (<= 0x1DC0 cp 0x1DFF) 0   ;; combining supplements
    (<= 0xFE20 cp 0xFE2F) 0   ;; combining half marks
    (<= 0x1100 cp 0x115F) 2   ;; Hangul Jamo
    (<= 0x2E80 cp 0x9FFF) 2   ;; CJK
    (<= 0xF900 cp 0xFAFF) 2   ;; CJK compat
    (<= 0xFE30 cp 0xFE6F) 2   ;; CJK compat forms
    (<= 0xFF01 cp 0xFF60) 2   ;; fullwidth forms
    (<= 0x20000 cp 0x2FFFF) 2 ;; CJK extension B+
    :else 1))

(defn display-width
  "Total terminal columns for text[from..to)."
  ^long [^CharSequence t ^long from ^long to]
  (loop [p from w 0]
    (if (>= p to) w
      (let [cp (Character/codePointAt t (int p))]
        (recur (+ p (Character/charCount cp)) (+ w (char-width cp)))))))

(defn word-forward
  "Position after the next word boundary."
  ^long [^CharSequence t ^long p]
  (loop [p p in false]
    (if (>= p (.length t)) (.length t)
      (let [cp (Character/codePointAt t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (next-pos t p) wc))))))

(defn word-backward
  "Position before the previous word boundary."
  ^long [^CharSequence t ^long p]
  (loop [p p in false]
    (if (<= p 0) 0
      (let [cp (Character/codePointBefore t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (prev-pos t p) wc))))))

(defn pos-at-col
  "Position in t where display column reaches col, scanning from..to."
  ^long [^CharSequence t ^long from ^long to ^long col]
  (loop [p from w 0]
    (if (or (>= p to) (>= w col)) p
      (let [cp (Character/codePointAt t (int p))
            cw (char-width cp)
            nw (+ w cw)]
        (if (> nw col) p
          (recur (+ p (Character/charCount cp)) nw))))))

(defn search-forward
  "Position of pattern in t at or after from, or nil."
  [^CharSequence t ^String pattern ^long from]
  (when (seq pattern)
    (let [tn (.length t)
          pn (.length pattern)]
      (loop [i from]
        (when (<= (+ i pn) tn)
          (if (loop [j (int 0)]
                (cond
                  (>= j pn) true
                  (= (.charAt t (int (+ i j))) (.charAt pattern j)) (recur (unchecked-inc-int j))
                  :else false))
            i
            (recur (inc i))))))))

(defn search-backward
  "Position of pattern in t before from, or nil."
  [^CharSequence t ^String pattern ^long from]
  (when (seq pattern)
    (let [pn    (.length pattern)
          start (min (dec from) (- (.length t) pn))]
      (loop [i (long (max 0 start))]
        (when (>= i 0)
          (if (loop [j (int 0)]
                (cond
                  (>= j pn) true
                  (= (.charAt t (int (+ i j))) (.charAt pattern j)) (recur (unchecked-inc-int j))
                  :else false))
            i
            (recur (dec i))))))))
