(ns xmas.buf
  (:require [xmas.gap :as gap]))

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
    :modified false :mode :fundamental :undo [] :redo [] :hscroll 0}))

(defn set-point [b f]
  (assoc b :point (-> (f (:text b) (:point b))
                      (max 0) (min (count (:text b))))))

(defn edit [b from to repl]
  (let [t     (:text b)
        old   (.toString (.subSequence ^CharSequence t (int from) (int to)))
        delta (- (count repl) (- to from))]
    (-> b
        (assoc :text (gap/edit t from to repl))
        (assoc :modified true)
        (update :undo bounded-conj undo-limit {:from from :old old :new repl})
        (assoc :redo [])
        (update :point (fn [p]
          (cond (< p from) p
                (<= p to)  (+ from (count repl))
                :else      (+ p delta)))))))

(defn- replay
  "Pop the latest entry from src-key, swap its `gone` text for its `kept` text,
   move point to the tail of the kept text, and push the entry onto dst-key."
  [b src-key dst-key gone-key kept-key]
  (if-let [{:keys [from] :as entry} (peek (get b src-key))]
    (let [to   (+ from (count (gone-key entry)))
          kept (kept-key entry)]
      (-> b
          (assoc :text (gap/edit (:text b) from to kept))
          (update src-key pop)
          (update dst-key bounded-conj undo-limit entry)
          (assoc :point (+ from (count kept)))))
    b))

(defn undo [b] (replay b :undo :redo :new :old))
(defn redo [b] (replay b :redo :undo :old :new))
