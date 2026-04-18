(ns xmas.view
  (:require [clojure.string :as str]
            [xmas.face :as face]
            [xmas.gap :as gap]
            [xmas.overlay :as ov]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.window :as win]))

(def faces
  {:default   {:fg nil :bg nil :bold false}
   :modeline  {:fg 235 :bg 108 :bold true}
   :inactive  {:fg 241 :bg 236 :bold false}
   :prompt    {:fg 109 :bg nil :bold true}
   :region    {:fg nil :bg 24  :bold false}
   :divider   {:fg 240 :bg nil :bold false}
   :comment   {:fg 102 :bg nil :bold false}
   :string    {:fg 107 :bg nil :bold false}
   :keyword   {:fg 180 :bg nil :bold false}
   :builtin   {:fg 67  :bg nil :bold true}})

(defn- ensure-visible [content point scroll rows]
  (let [point-line  (gap/line-of content point)
        scroll-line (gap/line-of content scroll)]
    (cond
      (< point scroll)
      (gap/nth-line-start content point-line)

      (>= (- point-line scroll-line) rows)
      (gap/nth-line-start content (- point-line (dec rows)))

      :else scroll)))

(defn- ensure-hvisible ^long [^long hscroll ^long cc ^long cols]
  ;; When hscroll > 0, column 0 is reserved for the `$` marker, so only
  ;; (cols - 1) columns of text are visible. The cursor is placed at
  ;; col-in-win = cc - hscroll + 1, so it fits iff cc < hscroll + cols - 1.
  ;; On overflow, pick hscroll so cc lands on the last visible column.
  (cond
    (< cc hscroll) cc
    (>= cc (+ hscroll cols (if (pos? hscroll) -1 0))) (+ cc 2 (- cols))
    :else hscroll))

;; --- Low-level writing within a rect ---

(defn- pad-spaces [n] (apply str (repeat n \space)))

(defn- write-spans!
  "Write row at (screen-row, screen-col) with face spans. Content is clipped
   to `width` columns (at most), and remaining columns are padded with spaces.
   `hscroll` is the horizontal offset into glyphs (0 for modeline etc.)."
  [screen-row screen-col width glyphs spans hscroll]
  (t/move screen-row screen-col)
  (let [n (count glyphs)
        scrolled (pos? hscroll)
        start (if scrolled (text/pos-at-col glyphs 0 n hscroll) 0)
        avail (if scrolled (dec width) width)
        end   (text/pos-at-col glyphs start n avail)
        ;; visible display width used
        used-cols (cond-> (text/display-width glyphs start end)
                    scrolled inc)]
    (when scrolled (t/sg (:default faces)) (t/tw "$"))
    (doseq [[from to face] spans]
      (let [a (max (long from) start)
            b (min (long to) end)]
        (when (< a b)
          (t/sg (get faces face (:default faces)))
          (t/tw (subs glyphs a b)))))
    (t/reset-sg)
    (when (< used-cols width)
      (t/tw (pad-spaces (- width used-cols))))))

(defn- fill-row!
  "Emit `width` spaces with `face` at (screen-row, screen-col). Used to clear
   trailing window rows and to paint dividers."
  [screen-row screen-col width face]
  (t/move screen-row screen-col)
  (t/sg (get faces face (:default faces)))
  (t/tw (pad-spaces width))
  (t/reset-sg))

(defn apply-face-range
  "Split `spans` at [from, to), painting that slice with `overlay-face`.
   Returns spans unchanged for nil or empty ranges."
  [spans from to overlay-face]
  (if (or (nil? from) (nil? to) (>= (long from) (long to)))
    spans
    (let [lo (long from) hi (long to)]
      (reduce
        (fn [acc [a b orig]]
          (let [a (long a) b (long b)]
            (if (or (<= b lo) (>= a hi))
              (conj acc [a b orig])
              (let [ma (max a lo) mb (min b hi)]
                (cond-> acc
                  (< a ma)   (conj [a ma orig])
                  (< ma mb)  (conj [ma mb overlay-face])
                  (< mb b)   (conj [mb b orig]))))))
        []
        spans))))

(defn merge-region
  "Split spans at region boundaries, setting :region face for parts inside
   [r-lo, r-hi). Bounds are line-relative char indices."
  [spans r-lo r-hi]
  (apply-face-range spans r-lo r-hi :region))

(defn- apply-overlays
  "Overlay each buffer-global overlay onto line-local spans. Overlays are
   sorted by :priority ascending so higher-priority overlays paint last and
   thus win visually."
  [spans overlays line-pos line-len]
  (reduce
    (fn [acc ov]
      (let [from-local (max 0 (- (:from ov) line-pos))
            to-local   (min line-len (- (:to ov) line-pos))]
        (if (< from-local to-local)
          (apply-face-range acc from-local to-local (:face ov))
          acc)))
    spans (sort-by #(or (:priority %) 0) overlays)))

;; --- Window rendering ---

(defn- line-str [^CharSequence content ^long pos ^long eol]
  (.toString (.subSequence content (int pos) (int eol))))

(defn- extend-states
  "Ensure `states` has entries up through line (target-line - 1). Entry i is
   the tokenizer state *after* line i (so it's the starting state for line
   i+1). Returns [new-states state-before-target-line]."
  [mode content states ^long target-line]
  (if (zero? target-line)
    [states :normal]
    (let [n (count states)]
      (if (>= n target-line)
        [states (nth states (dec target-line))]
        (loop [ln n
               state (if (zero? n) :normal (nth states (dec n)))
               out states]
          (if (>= ln target-line)
            [out state]
            (let [pos (gap/nth-line-start content ln)
                  eol (gap/nth-line-end content ln)
                  [_ nxt] (face/tokenize mode (line-str content pos eol) state)]
              (recur (inc ln) nxt (conj out nxt)))))))))

(defn- render-window-body
  "Render visible lines and return the possibly-extended :line-states cache."
  [rect content mode overlays scroll hscroll r-lo r-hi states]
  (let [{:keys [row col rows cols]} rect
        body-rows (dec rows)
        start-line  (gap/line-of content scroll)
        total-lines (gap/line-count content)
        [states0 init-state] (extend-states mode content (or states []) start-line)
        result
        (loop [ln start-line r 0 state init-state states states0]
          (if (and (< r body-rows) (< ln total-lines))
            (let [pos  (gap/nth-line-start content ln)
                  eol  (gap/nth-line-end content ln)
                  line (line-str content pos eol)
                  len  (count line)
                  [toks nxt-state] (face/tokenize mode line state)
                  line-ovs (ov/in-range overlays pos eol)
                  spans0 (apply-overlays toks line-ovs pos len)
                  r-from (when r-lo (max 0 (min len (- (long r-lo) pos))))
                  r-to   (when r-lo (max 0 (min len (- (long r-hi) pos))))
                  spans  (merge-region spans0 r-from r-to)
                  states' (if (> (count states) ln) states (conj states nxt-state))]
              (write-spans! (+ row r) col cols line spans hscroll)
              (recur (inc ln) (inc r) nxt-state states'))
            {:r r :states states}))]
    (loop [r (:r result)]
      (when (< r body-rows)
        (fill-row! (+ row r) col cols :default)
        (recur (inc r))))
    (:states result)))

(defn- render-window-modeline
  [rect buf-name modified mode minor-modes point-line cursor-col current?]
  (let [{:keys [row col rows cols]} rect
        minors (when (seq minor-modes)
                 (str " " (str/join " " (map clojure.core/name (sort minor-modes)))))
        ml (format " %s %s   L%d C%d  (%s%s)"
                   (if modified "**" "--") (or buf-name "")
                   (inc point-line) (inc cursor-col)
                   (clojure.core/name (or mode :fundamental))
                   (or minors ""))
        mw (text/display-width ml 0 (count ml))
        padded (if (< mw cols)
                 (str ml (pad-spaces (- cols mw)))
                 (subs ml 0 (text/pos-at-col ml 0 (count ml) cols)))
        face (if current? :modeline :inactive)]
    (write-spans! (+ row (dec rows)) col cols padded [[0 (count padded) face]] 0)))

(defn- render-one-window
  "Render one window into its rect. Returns
   {:window <updated window with scroll/hscroll/point-line/cursor-col>
    :buf-name <buffer key>
    :version <buffer version at render time>
    :states <possibly-extended :line-states cache>}."
  [state window rect current?]
  (let [{:keys [rows cols]} rect
        body-rows (dec rows)
        buf-name (:buffer window)
        buf (get (:bufs state) buf-name)
        content (:text buf)
        point (:point buf)
        mark (:mark buf)
        mode (:mode buf)
        scroll-in (:scroll window 0)
        hscroll-in (:hscroll window 0)
        scroll (if current? (ensure-visible content point scroll-in body-rows) scroll-in)
        point-line (gap/line-of content point)
        cursor-col (text/display-width content (gap/nth-line-start content point-line) point)
        hscroll (if current? (ensure-hvisible hscroll-in cursor-col cols) hscroll-in)
        [r-lo r-hi] (when (and mark current?) (sort [(long mark) (long point)]))
        overlays (:overlays buf)
        states (render-window-body rect content mode overlays scroll hscroll
                                   r-lo r-hi (:line-states buf))]
    (render-window-modeline rect (:name buf) (:modified buf) mode (:minor-modes buf)
                            point-line cursor-col current?)
    {:window (assoc window :scroll scroll :hscroll hscroll
                           :point-line point-line :cursor-col cursor-col)
     :buf-name buf-name
     :version (:version buf)
     :states states}))

;; --- Dividers ---

(defn- paint-divider! [{:keys [kind row col len]}]
  (case kind
    :horizontal (fill-row! row col len :divider)
    :vertical   (doseq [r (range len)]
                  (t/move (+ row r) col)
                  (t/sg (:divider faces))
                  (t/tw "│")
                  (t/reset-sg))))

;; --- Echo area ---

(defn- pad-to-cols! [used cols]
  (when (< used cols) (t/tw (pad-spaces (- cols used)))))

(defn- render-echo-area
  [{:keys [rows cols] :as state}]
  (let [mini (:mini state) isearch (:isearch state) msg (:msg state)
        mini-row (dec rows)]
    (t/move mini-row 0)
    (cond
      mini
      (let [{:keys [prompt]} mini
            mb (get (:bufs state) (:buf state))
            input (:text mb)
            cursor (:point mb)]
        (t/sg (:prompt faces)) (t/tw prompt)
        (t/reset-sg) (t/tw input)
        (pad-to-cols! (+ (count prompt) (text/display-width input 0 (count input))) cols)
        (t/move mini-row (+ (count prompt) (text/display-width input 0 cursor))))

      isearch
      (let [p (str (if (= (:direction isearch) :forward) "I-search: " "I-search backward: ")
                   (:pattern isearch))]
        (t/sg (:prompt faces)) (t/tw p) (t/reset-sg)
        (pad-to-cols! (text/display-width p 0 (count p)) cols))

      :else
      (let [m (or msg "")]
        (t/reset-sg) (t/tw m)
        (pad-to-cols! (text/display-width m 0 (count m)) cols)))))

;; --- Frame signature (dirty-flag input) ---

(defn- window-sig [state window current?]
  (let [buf (get (:bufs state) (:buffer window))]
    [(:buffer window) (:version buf) (:point buf) (:mark buf)
     (:mode buf) (:minor-modes buf) (:modified buf) (:name buf)
     (:overlays buf) (:scroll window) (:hscroll window) current?]))

(defn- echo-sig [state]
  (cond
    (:mini state)
    (let [mb (get (:bufs state) (:buf state))]
      [:mini (get-in state [:mini :prompt]) (str (:text mb)) (:point mb)])
    (:isearch state)
    [:isearch (:direction (:isearch state)) (:pattern (:isearch state))]
    :else [:msg (:msg state)]))

(defn- frame-sig [state layouts cur-path]
  [(:rows state) (:cols state) layouts cur-path
   (into {} (for [[path _] layouts]
              [path (window-sig state (get-in (:windows state) path)
                                (= path cur-path))]))
   (echo-sig state)])

;; --- Top-level render ---

(defn render
  "Render the whole frame. Returns {:sig :cur-scroll :cur-hscroll}. If the
   input signature equals (:render-sig state) the frame is skipped and only
   {:sig} is returned — the caller should write :sig back into state and
   leave scroll/hscroll untouched."
  [state]
  (let [rows (:rows state) cols (:cols state)
        window-rows (dec rows)   ; reserve last row for echo area
        tree (:windows state)
        layouts (win/layout tree window-rows cols)
        divs (win/dividers tree window-rows cols)
        cur-path (:cur-window state)
        sig (frame-sig state layouts cur-path)]
    (if (= sig (:render-sig state))
      {:sig sig}
      (let [rendered (volatile! {})
            cache-ups (volatile! {})]
        (t/with-frame
          (t/hide-cur)
          (doseq [[path rect] layouts]
            (let [w (get-in tree path)
                  {:keys [window buf-name version states]}
                  (render-one-window state w rect (= path cur-path))]
              (vswap! rendered assoc path window)
              (vswap! cache-ups
                      #(let [ex (get % buf-name)]
                         (if (and ex (>= (count (:states ex)) (count states)))
                           %
                           (assoc % buf-name
                                  {:version version :states states}))))))
          (doseq [d divs] (paint-divider! d))
          (render-echo-area state)
          (when-not (:mini state)
            (let [rect (get layouts cur-path)
                  win  (get @rendered cur-path)
                  content (:text (get (:bufs state) (:buffer win)))
                  scroll-line (gap/line-of content (:scroll win))
                  body-rows (dec (:rows rect))
                  row-in-win (- (:point-line win) scroll-line)
                  row-in-win (if (>= row-in-win body-rows) (dec body-rows) row-in-win)
                  col-in-win (cond-> (- (:cursor-col win) (:hscroll win))
                                     (pos? (:hscroll win)) inc)]
              (t/move (+ (:row rect) row-in-win) (+ (:col rect) col-in-win))))
          (t/show-cur))
        (let [cur-win (get @rendered cur-path)]
          {:sig sig
           :cur-scroll (:scroll cur-win)
           :cur-hscroll (:hscroll cur-win)
           :line-state-updates @cache-ups})))))
