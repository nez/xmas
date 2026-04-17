(ns xmas.view
  (:require [xmas.gap :as gap]
            [xmas.term :as t]
            [xmas.text :as text]))

(def faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :prompt    {:fg 109 :bg nil :bold true}})

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
  (let [scrolled (pos? hscroll)
        start (if scrolled (text/pos-at-col glyphs 0 (count glyphs) hscroll) 0)
        tail  (subs glyphs start)
        avail (if scrolled (dec cols) cols)
        end   (text/pos-at-col tail 0 (count tail) avail)]
    (when scrolled (t/tw "$"))
    (t/tw (subs tail 0 end)))
  (t/reset-sg)
  (t/clreol))

;; --- Layout: resolve scroll/hscroll/point-line/cursor-col up front. ---

(defn- layout [{:keys [buf bufs scroll rows cols mini isearch msg]}]
  (let [display-buf (if mini (:prev-buf mini) buf)
        {content :text :keys [point name modified mode hscroll]} (get bufs display-buf)
        hscroll    (or hscroll 0)
        body-rows  (- rows 2)
        scroll     (ensure-visible content point scroll body-rows)
        point-line (gap/line-of content point)
        cursor-col (text/display-width content (gap/nth-line-start content point-line) point)
        hscroll    (ensure-hvisible hscroll cursor-col cols)]
    {:content content :point point :name (or name display-buf)
     :modified modified :mode mode
     :scroll scroll :hscroll hscroll :rows rows :cols cols :body-rows body-rows
     :point-line point-line :cursor-col cursor-col
     :mini mini :isearch isearch :msg msg :bufs bufs :buf buf}))

;; --- Phases. Each consumes the layout map. ---

(defn- render-body [{:keys [content scroll hscroll body-rows cols]}]
  (let [start-line  (gap/line-of content scroll)
        total-lines (gap/line-count content)]
    (loop [ln start-line row 0]
      (when (and (< row body-rows) (< ln total-lines))
        (let [pos (gap/nth-line-start content ln)
              eol (gap/nth-line-end content ln)
              line (.toString (.subSequence ^CharSequence content (int pos) (int eol)))]
          (write-row! row line :default cols hscroll)
          (recur (inc ln) (inc row)))))))

(defn- render-modeline [{:keys [name modified mode point-line cursor-col body-rows cols]}]
  (let [ml (format " %s %s   L%d C%d  (%s)"
                   (if modified "**" "--") name
                   (inc point-line) (inc cursor-col)
                   (clojure.core/name (or mode :fundamental)))
        mw (text/display-width ml 0 (count ml))
        padded (if (< mw cols)
                 (str ml (apply str (repeat (- cols mw) \space)))
                 (subs ml 0 (text/pos-at-col ml 0 (count ml) cols)))]
    (write-row! body-rows padded :modeline cols 0)))

(defn- render-echo-area [{:keys [mini isearch msg bufs buf rows]}]
  (let [mini-row (dec rows)]
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
      (do (t/reset-sg) (t/tw (or msg "")) (t/clreol)))))

(defn- place-cursor [{:keys [content scroll point-line cursor-col hscroll body-rows mini]}]
  (when-not mini
    (let [row (- point-line (gap/line-of content scroll))
          cr (if (>= row body-rows) (dec body-rows) row)
          cc (cond-> (- cursor-col hscroll) (pos? hscroll) inc)]
      (t/move cr cc))))

(defn render [state]
  (let [ctx (layout state)]
    (t/with-frame
      (t/hide-cur)
      (render-body ctx)
      (render-modeline ctx)
      (render-echo-area ctx)
      (place-cursor ctx)
      (t/show-cur))
    (select-keys ctx [:scroll :hscroll])))
