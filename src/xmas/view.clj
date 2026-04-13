(ns xmas.view
  (:require [xmas.term :as t]
            [xmas.text :as text]))

(def faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :prompt    {:fg 109 :bg nil :bold true}})

(defn cursor-pos
  "Screen [row col] of point relative to scroll."
  [content point scroll rows]
  (loop [pos scroll row 0]
    (if (>= row rows)
      [(dec rows) 0]
      (let [eol (text/line-end content pos)
            nxt (min (inc eol) (count content))]
        (if (<= pos point eol)
          [row (text/display-width content pos point)]
          (recur nxt (inc row)))))))

(defn- ensure-visible
  "Return adjusted scroll so point is on screen."
  [content point scroll rows]
  (cond
    (< point scroll)
    (text/line-start content point)

    (>= (first (cursor-pos content point scroll rows)) rows)
    (loop [s scroll]
      (let [ns (min (inc (text/line-end content s)) (count content))]
        (if (or (< (first (cursor-pos content point ns rows)) rows)
                (>= ns (count content)))
          ns (recur ns))))

    :else scroll))

(defn- write-row! [row glyphs face cols]
  (t/move row 0)
  (t/sg (get faces face (:default faces)))
  (let [end (text/pos-at-col glyphs 0 (count glyphs) cols)]
    (t/tw (subs glyphs 0 end)))
  (t/reset-sg)
  (t/clreol))

(defn render [{:keys [buf bufs scroll rows cols msg mini]}]
  (let [display-buf (if mini (:prev-buf mini) buf)
        {content :text :keys [point name modified mode]} (get bufs display-buf)
        body-rows (- rows 2)
        scroll (ensure-visible content point scroll body-rows)
        mini-row (dec rows)]
    (t/hide-cur)
    ;; text
    (loop [pos scroll row 0]
      (when (< row body-rows)
        (let [eol (text/line-end content pos)
              line (subs content pos eol)
              nxt (min (inc eol) (count content))]
          (write-row! row line :default cols)
          (recur nxt (inc row)))))
    ;; mode line
    (let [ml (format " %s %s   (%s)"
                     (if modified "**" "--") (or name display-buf)
                     (clojure.core/name (or mode :fundamental)))
          mw (text/display-width ml 0 (count ml))
          padded (if (< mw cols)
                   (str ml (apply str (repeat (- cols mw) \space)))
                   (subs ml 0 (text/pos-at-col ml 0 (count ml) cols)))]
      (write-row! body-rows padded :modeline cols))
    ;; minibuffer / echo area
    (t/move mini-row 0)
    (if mini
      (let [{:keys [prompt]} mini
            mb (get bufs buf)
            input (:text mb)
            cursor (:point mb)]
        (t/sg (:prompt faces)) (t/tw prompt)
        (t/reset-sg) (t/tw input) (t/clreol)
        (t/move mini-row (+ (count prompt) (text/display-width input 0 cursor))))
      (do (t/reset-sg) (t/tw (or msg "")) (t/clreol)))
    ;; cursor
    (when-not mini
      (let [[cr cc] (cursor-pos content point scroll body-rows)]
        (t/move cr cc)))
    (t/show-cur)
    (t/flush!)
    scroll))
