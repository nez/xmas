(ns xmas.buf
  (:require [xmas.gap :as gap]
            [xmas.overlay :as ov]))

(def undo-limit 1000)

(defn bounded-conj
  "conj v onto vec, dropping the oldest entry once size exceeds limit."
  [vec limit v]
  (let [c (conj vec v)]
    (if (> (count c) limit) (subvec c 1) c)))

(defn make
  ([name] (make name "" nil))
  ([name text file]
   {:name name :text (gap/of text) :point 0 :mark nil :file file
    :modified false :mode :fundamental :undo [] :redo [] :version 0}))

(defn set-point [b f]
  (assoc b :point (-> (f (:text b) (:point b))
                      (max 0) (min (count (:text b))))))

(defn- shift-pos
  "Shift a position to reflect an edit [from,to) → text of `repl-len` chars."
  [^long p ^long from ^long to ^long repl-len]
  (cond (< p from) p
        (<= p to)  (+ from repl-len)
        :else      (+ p (- repl-len (- to from)))))

(defn- truncate-states
  "Drop tokenizer-state cache entries at or beyond the edit line. The entry
   at index i is the state *after* line i — entries for lines ≥ from-line
   could change once that line is modified."
  [states from-line]
  (let [n (count (or states []))
        keep (min n (max 0 from-line))]
    (if (zero? keep) [] (subvec states 0 keep))))

(defn edit [b from to repl]
  (let [t         (:text b)
        ;; Normalize: callers sometimes pass out-of-order or out-of-bounds
        ;; ranges (query-replace paths, dired ops). Clamp once here so the
        ;; underlying gap/edit never sees a bad range.
        tn        (count t)
        from      (-> (long from) (max 0) (min tn))
        to        (-> (long to)   (max from) (min tn))
        repl-len  (count repl)]
    (if (and (= from to) (zero? repl-len))
      ;; No-op: don't mark modified, don't pollute undo/redo. A zero-width
      ;; insert of "" used to clear redo and bump :modified spuriously.
      b
      (let [old       (.toString (.subSequence ^CharSequence t (int from) (int to)))
            new-text  (gap/edit t from to repl)
            from-line (gap/line-of new-text from)]
        (-> b
            (assoc :text new-text)
            (assoc :modified true)
            (update :edit-count (fnil inc 0))
            (update :version (fnil inc 0))
            (update :line-states truncate-states from-line)
            (update :overlays #(ov/adjust (or % []) from to repl-len))
            (update :undo bounded-conj undo-limit {:from from :old old :new repl})
            (assoc :redo [])
            (update :point shift-pos from to repl-len)
            (update :mark #(when % (shift-pos % from to repl-len))))))))

(defn- replay
  "Pop the latest entry from src-key, swap its `gone` text for its `kept` text,
   move point to the tail of the kept text, and push the entry onto dst-key."
  [b src-key dst-key gone-key kept-key]
  (if-let [{:keys [from] :as entry} (peek (get b src-key))]
    (let [to       (+ from (count (gone-key entry)))
          kept     (kept-key entry)
          kept-len (count kept)
          new-text (gap/edit (:text b) from to kept)
          from-line (gap/line-of new-text from)]
      (-> b
          (assoc :text new-text)
          (assoc :modified true)
          ;; Undo/redo mutate the buffer, so they must participate in
          ;; auto-save threshold tracking — otherwise a long undo chain
          ;; never gets its state backed up.
          (update :edit-count (fnil inc 0))
          (update :version (fnil inc 0))
          (update :line-states truncate-states from-line)
          (update src-key pop)
          (update dst-key bounded-conj undo-limit entry)
          (update :overlays #(ov/adjust (or % []) from to kept-len))
          (update :mark #(when % (shift-pos % from to kept-len)))
          (assoc :point (+ from kept-len))))
    b))

(defn undo [b] (replay b :undo :redo :new :old))
(defn redo [b] (replay b :redo :undo :old :new))
