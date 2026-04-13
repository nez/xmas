(ns xmas.view
  (:require [xmas.gap :as gap]
            [xmas.term :as t]
            [xmas.text :as text]))

(def faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :prompt    {:fg 109 :bg nil :bold true}})

(defn cursor-pos
  "Screen [row col] of point relative to scroll."
  [content point scroll rows]
  (let [scroll-line (gap/line-of content scroll)
        point-line  (gap/line-of content point)
        row (- point-line scroll-line)]
    (if (>= row rows)
      [(dec rows) 0]
      [row (text/display-width content (gap/nth-line-start content point-line) point)])))

(defn- ensure-visible
  "Return adjusted scroll so point is on screen."
  [content point scroll rows]
  (let [point-line  (gap/line-of content point)
        scroll-line (gap/line-of content scroll)]
    (cond
      (< point scroll)
      (gap/nth-line-start content point-line)

      (>= (- point-line scroll-line) rows)
      (gap/nth-line-start content (- point-line (dec rows)))

      :else scroll)))

(defn- ensure-hvisible
  "Adjust hscroll so cursor column cc is visible within cols."
  ^long [^long hscroll ^long cc ^long cols]
  (cond
    (< cc hscroll) cc
    (>= cc (+ hscroll cols)) (inc (- cc cols))
    :else hscroll))

(defn- write-row! [row glyphs face cols hscroll]
  (t/move row 0)
  (t/sg (get faces face (:default faces)))
  (if (pos? hscroll)
    (let [start (text/pos-at-col glyphs 0 (count glyphs) hscroll)
          visible (subs glyphs start)
          end (text/pos-at-col visible 0 (count visible) (dec cols))]
      (t/tw "$")
      (t/tw (subs visible 0 end)))
    (let [end (text/pos-at-col glyphs 0 (count glyphs) cols)]
      (t/tw (subs glyphs 0 end))))
  (t/reset-sg)
  (t/clreol))

(defn render [{:keys [buf bufs scroll rows cols msg mini isearch]}]
  (let [display-buf (if mini (:prev-buf mini) buf)
        {content :text :keys [point name modified mode hscroll]} (get bufs display-buf)
        hscroll (or hscroll 0)
        body-rows (- rows 2)
        scroll (ensure-visible content point scroll body-rows)
        point-line (gap/line-of content point)
        cursor-col (text/display-width content (gap/nth-line-start content point-line) point)
        hscroll (ensure-hvisible hscroll cursor-col cols)
        mini-row (dec rows)]
    (t/with-frame
      (t/hide-cur)
      ;; text
      (let [start-line (gap/line-of content scroll)
            total-lines (gap/line-count content)]
        (loop [ln start-line row 0]
          (when (and (< row body-rows) (< ln total-lines))
            (let [pos (gap/nth-line-start content ln)
                  eol (gap/nth-line-end content ln)
                  line (.toString (.subSequence ^CharSequence content (int pos) (int eol)))]
              (write-row! row line :default cols hscroll)
              (recur (inc ln) (inc row))))))
      ;; mode line
      (let [line (inc point-line)
            col (inc cursor-col)
            ml (format " %s %s   L%d C%d  (%s)"
                       (if modified "**" "--") (or name display-buf)
                       line col
                       (clojure.core/name (or mode :fundamental)))
            mw (text/display-width ml 0 (count ml))
            padded (if (< mw cols)
                     (str ml (apply str (repeat (- cols mw) \space)))
                     (subs ml 0 (text/pos-at-col ml 0 (count ml) cols)))]
        (write-row! body-rows padded :modeline cols 0))
      ;; minibuffer / echo area
      (t/move mini-row 0)
      (cond
        mini
        (let [{:keys [prompt]} mini
              mb (get bufs buf)
              input (:text mb)
              cursor (:point mb)]
          (t/sg (:prompt faces)) (t/tw prompt)
          (t/reset-sg) (t/tw input) (t/clreol)
          (t/move mini-row (+ (count prompt) (text/display-width input 0 cursor))))

        isearch
        (let [prompt (str (if (= (:direction isearch) :forward) "I-search: " "I-search backward: ")
                          (:pattern isearch))]
          (t/sg (:prompt faces)) (t/tw prompt) (t/reset-sg) (t/clreol))

        :else
        (do (t/reset-sg) (t/tw (or msg "")) (t/clreol)))
      ;; cursor
      (when-not mini
        (let [[cr _] (cursor-pos content point scroll body-rows)
              cc (- cursor-col hscroll)
              cc (if (pos? hscroll) (inc cc) cc)]
          (t/move cr cc)))
      (t/show-cur))
    {:scroll scroll :hscroll hscroll}))
