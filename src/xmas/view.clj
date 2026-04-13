(ns xmas.view
  (:require [xmas.term :as t]))

;; Faces
(def faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :prompt    {:fg 109 :bg nil :bold true}})

;; --- Rendering helpers ---

(defn line-start [text pos]
  (let [i (.lastIndexOf text (int \newline) (int (max 0 (dec pos))))]
    (if (neg? i) 0 (inc i))))

(defn line-end [text pos]
  (let [i (.indexOf text (int \newline) (int pos))]
    (if (neg? i) (count text) i)))

(defn cursor-pos
  "Screen [row col] of point relative to scroll."
  [text point scroll rows]
  (loop [pos scroll row 0]
    (if (>= row rows)
      [(dec rows) 0]
      (let [eol (line-end text pos)
            nxt (min (inc eol) (count text))]
        (if (<= pos point eol)
          [row (- point pos)]
          (recur nxt (inc row)))))))

(defn ensure-visible
  "Return adjusted scroll so point is on screen."
  [text point scroll rows]
  (cond
    (< point scroll)
    (line-start text point)

    (>= (first (cursor-pos text point scroll rows)) rows)
    (loop [s scroll]
      (let [ns (min (inc (line-end text s)) (count text))]
        (if (or (< (first (cursor-pos text point ns rows)) rows)
                (>= ns (count text)))
          ns (recur ns))))

    :else scroll))

;; --- Write one row ---

(defn write-row! [row glyphs face cols]
  (t/move row 0)
  (t/sg (get faces face (:default faces)))
  (let [n (min (count glyphs) cols)]
    (t/tw (subs glyphs 0 n)))
  (t/reset-sg)
  (t/clreol))

;; --- Render entire screen from editor state ---

(defn render [{:keys [buf bufs scroll rows cols msg mini]}]
  (let [;; when mini active, display the prev buffer's text, not the mini buffer
        display-buf (if mini (:prev-buf mini) buf)
        {:keys [text point name modified mode]} (get bufs display-buf)
        body-rows (- rows 2)
        scroll (ensure-visible text point scroll body-rows)
        mini-row (dec rows)]
    (t/hide-cur)
    ;; text lines
    (loop [pos scroll row 0]
      (when (< row body-rows)
        (let [eol (line-end text pos)
              line (subs text pos (min eol (+ pos cols)))
              nxt (min (inc eol) (count text))]
          (write-row! row line :default cols)
          (recur nxt (inc row)))))
    ;; mode line
    (let [ml (format " %s %s   (%s)"
                     (if modified "**" "--") name
                     (clojure.core/name (or mode :fundamental)))
          padded (if (< (count ml) cols)
                   (str ml (apply str (repeat (- cols (count ml)) \space)))
                   (subs ml 0 cols))]
      (write-row! body-rows padded :modeline cols))
    ;; minibuffer line
    (t/move mini-row 0)
    (if mini
      (let [{:keys [prompt]} mini
            mb (get bufs buf)  ;; current buf IS the mini buffer
            input (:text mb)
            cursor (:point mb)]
        (t/sg (:prompt faces)) (t/tw prompt)
        (t/reset-sg) (t/tw input) (t/clreol)
        (t/move mini-row (+ (count prompt) cursor)))
      (do (t/reset-sg) (t/tw (or msg "")) (t/clreol)))
    ;; cursor
    (when-not mini
      (let [[cr cc] (cursor-pos text point scroll body-rows)]
        (t/move cr cc)))
    (t/show-cur)
    (t/flush!)
    scroll))
