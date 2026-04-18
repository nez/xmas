(ns xmas.overlay
  "Buffer overlays: arbitrary [from to face props] ranges that move with
   edits and layer onto the rendered face spans.")

(defn make
  ([from to] (make from to :default {}))
  ([from to face] (make from to face {}))
  ([from to face props]
   (merge {:face face} props {:from from :to to})))

(defn- adjust-range
  "Shift/shrink a single [from to] range to reflect an edit at [e-from e-to)
   being replaced with text of length `repl-len`."
  [from to e-from e-to repl-len]
  (let [delta (- repl-len (- e-to e-from))]
    (cond
      ;; entirely before the edit — unchanged
      (<= to e-from) [from to]
      ;; entirely after the edit — shift by delta
      (>= from e-to) [(+ from delta) (+ to delta)]
      ;; edit fully contains the overlay — collapse at edit start
      (and (<= e-from from) (>= e-to to)) [e-from e-from]
      ;; edit overlaps the start
      (and (<= e-from from) (< e-to to)) [e-from (+ to delta)]
      ;; edit overlaps the end
      (and (> e-from from) (>= e-to to)) [from e-from]
      ;; edit is fully inside the overlay — stretch
      :else [from (+ to delta)])))

(defn adjust
  "Update overlay positions after an edit at [e-from e-to) replaced with
   `repl-len` characters. Drops zero-length overlays left behind."
  [overlays e-from e-to repl-len]
  (->> overlays
       (map (fn [ov]
              (let [[f t] (adjust-range (:from ov) (:to ov) e-from e-to repl-len)]
                (assoc ov :from f :to t))))
       (remove (fn [ov] (>= (:from ov) (:to ov))))
       vec))

(defn in-range
  "Return overlays that intersect the buffer range [from to)."
  [overlays from to]
  (filter (fn [ov]
            (and (< (:from ov) to) (> (:to ov) from)))
          overlays))
