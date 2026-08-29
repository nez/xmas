(ns xmas.rect
  "Rectangle operations: a 2D range defined by point and mark. Each command
   iterates lines in [top..bottom], operating on display columns in
   [left..right] per line. Lines shorter than `right` are space-padded so
   the rectangle stays a true 2D range. Bottoms out on cmd/edit + gap +
   text — no new buffer abstraction."
  (:require [xmas.cmd :as cmd]
            [xmas.gap :as gap]
            [xmas.text :as text]))

(defn- pos-col
  "Display column of buffer position pos."
  [t pos]
  (let [ln (gap/line-of t pos)
        ls (gap/nth-line-start t ln)]
    (text/display-width t ls pos)))

(defn coords
  "Return [top-line bottom-line left-col right-col] for the rectangle defined
   by point and mark, or nil if no mark."
  [s]
  (let [b (cmd/cur s) p (:point b) m (:mark b) t (:text b)]
    (when m
      (let [pl (gap/line-of t p) ml (gap/line-of t m)
            pc (pos-col t p)     mc (pos-col t m)]
        [(min pl ml) (max pl ml) (min pc mc) (max pc mc)]))))

(defn- line-bounds-at-cols
  "Return [lstart lend pad] for `line` at columns [left..right]:
   - lstart: buffer position of the left edge of the rect on this line
   - lend:   buffer position of the right edge
   - pad:    how many spaces of padding need to be inserted at lend to
             reach `right` (>0 only when the line is shorter than right)."
  [t line left right]
  (let [ls (gap/nth-line-start t line)
        le (gap/nth-line-end t line)
        line-w (text/display-width t ls le)
        lstart (text/pos-at-col t ls le (min left line-w))
        lend   (text/pos-at-col t ls le (min right line-w))
        pad    (max 0 (- right line-w))]
    [lstart lend pad]))

(defn- per-line
  "Apply f to each line's rectangle fragment, returning the new state. f
   receives the fragment as a String and returns a String replacement.
   Lines that don't reach the right edge are padded with spaces first."
  [s f]
  (if-let [[t-line b-line lc rc] (coords s)]
    (loop [s s line t-line]
      (if (> line b-line)
        s
        (let [t (:text (cmd/cur s))
              [_ le pad] (line-bounds-at-cols t line lc rc)
              s (if (pos? pad)
                  (cmd/edit s le le (.repeat " " pad))
                  s)
              t (:text (cmd/cur s))
              [lstart lend _] (line-bounds-at-cols t line lc rc)
              fragment (str (gap/substr t lstart lend))
              replacement (str (f fragment))]
          (recur (cmd/edit s lstart lend replacement) (inc line)))))
    s))

(defn extract
  "Return a vector of the rectangle's per-line strings without modifying state."
  [s]
  (when-let [[t-line b-line lc rc] (coords s)]
    (let [t (:text (cmd/cur s))]
      (mapv (fn [line]
              (let [[lstart lend pad] (line-bounds-at-cols t line lc rc)
                    frag (str (gap/substr t lstart lend))]
                (if (pos? pad)
                  (str frag (.repeat " " pad))
                  frag)))
            (range t-line (inc b-line))))))

(defn kill-rectangle
  "Capture the rectangle into :killed-rect on state, then erase it."
  [s]
  (if-let [_ (coords s)]
    (let [rect (extract s)]
      (-> s (per-line (constantly ""))
            (assoc :killed-rect rect)
            (cmd/update-cur #(assoc % :mark nil))))
    (cmd/msg s "No region")))

(defn clear-rectangle
  "Replace each line's rectangle fragment with spaces of the same width."
  [s]
  (per-line s #(.repeat " " (text/display-width %))))

(defn string-rectangle
  "Replace the rectangle on each line with `str`."
  [s str]
  (per-line s (constantly str)))

(defn copy-rectangle
  "Stash the rectangle on :killed-rect without modifying the buffer."
  [s]
  (if-let [r (extract s)]
    (cmd/msg (assoc s :killed-rect r) (str "Copied " (count r) " lines"))
    (cmd/msg s "No region")))

(defn yank-rectangle
  "Paste :killed-rect at point. Each stored line is inserted on a successive
   buffer line at the same column as point. Buffer lines too short are
   space-padded; if there aren't enough lines, newlines are appended first."
  [s]
  (if-let [rect (:killed-rect s)]
    (let [b (cmd/cur s) p (:point b) t (:text b)
          start-line (gap/line-of t p)
          start-col  (pos-col t p)
          n (count rect)]
      (loop [s s i 0]
        (if (>= i n)
          s
          (let [t (:text (cmd/cur s))
                line (+ start-line i)
                ;; Ensure the line exists by appending newlines.
                lc (gap/line-count t)
                s (if (>= line lc)
                    (let [pad (.repeat "\n" (- (inc line) lc))]
                      (cmd/edit s (count t) (count t) pad))
                    s)
                t (:text (cmd/cur s))
                ls (gap/nth-line-start t line)
                le (gap/nth-line-end t line)
                line-w (text/display-width t ls le)
                ;; Pad with spaces if the line is too short to reach start-col.
                s (if (< line-w start-col)
                    (cmd/edit s le le (.repeat " " (- start-col line-w)))
                    s)
                t (:text (cmd/cur s))
                ls (gap/nth-line-start t line)
                le (gap/nth-line-end t line)
                ins (text/pos-at-col t ls le start-col)]
            (recur (cmd/edit s ins ins (nth rect i)) (inc i))))))
    (cmd/msg s "No rectangle")))
