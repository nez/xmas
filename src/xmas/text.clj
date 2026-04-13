(ns xmas.text)

(defn next-pos
  "Advance past one codepoint."
  ^long [^String t ^long p]
  (if (>= p (.length t)) p
    (+ p (Character/charCount (.codePointAt t (int p))))))

(defn prev-pos
  "Retreat past one codepoint."
  ^long [^String t ^long p]
  (if (<= p 0) 0
    (- p (Character/charCount (.codePointBefore t (int p))))))

(defn line-start
  "Start of line containing pos."
  ^long [^String t ^long pos]
  (let [i (.lastIndexOf t (int \newline) (int (max 0 (dec pos))))]
    (if (neg? i) 0 (inc i))))

(defn line-end
  "End of line containing pos (position of newline, or end of text)."
  ^long [^String t ^long pos]
  (let [i (.indexOf t (int \newline) (int pos))]
    (if (neg? i) (count t) i)))

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
  ^long [^String t ^long from ^long to]
  (loop [p from w 0]
    (if (>= p to) w
      (let [cp (.codePointAt t (int p))]
        (recur (+ p (Character/charCount cp)) (+ w (char-width cp)))))))

(defn word-forward
  "Position after the next word boundary."
  ^long [^String t ^long p]
  (loop [p p in false]
    (if (>= p (count t)) (count t)
      (let [cp (.codePointAt t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (next-pos t p) wc))))))

(defn word-backward
  "Position before the previous word boundary."
  ^long [^String t ^long p]
  (loop [p p in false]
    (if (<= p 0) 0
      (let [cp (.codePointBefore t (int p))
            wc (Character/isLetterOrDigit cp)]
        (if (and in (not wc)) p
          (recur (prev-pos t p) wc))))))

(defn pos-at-col
  "Position in t where display column reaches col, scanning from..to."
  ^long [^String t ^long from ^long to ^long col]
  (loop [p from w 0]
    (if (or (>= p to) (>= w col)) p
      (let [cp (.codePointAt t (int p))
            cw (char-width cp)
            nw (+ w cw)]
        (if (> nw col) p
          (recur (+ p (Character/charCount cp)) nw))))))
