(ns xmas.buf
  (:require [xmas.gap :as gap]))

(def undo-limit 1000)

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
        (update :undo #(let [u (conj % {:from from :old old :new repl})]
                          (if (> (count u) undo-limit) (subvec u 1) u)))
        (assoc :redo [])
        (update :point (fn [p]
          (cond (< p from) p
                (<= p to)  (+ from (count repl))
                :else      (+ p delta)))))))

(defn- cap [v]
  (if (> (count v) undo-limit) (subvec v 1) v))

(defn undo [b]
  (if-let [{:keys [from old new]} (peek (:undo b))]
    (let [to (+ from (count new))]
      (-> b
          (assoc :text (gap/edit (:text b) from to old))
          (update :undo pop)
          (update :redo #(cap (conj % {:from from :old old :new new})))
          (assoc :point (+ from (count old)))))
    b))

(defn redo [b]
  (if-let [{:keys [from old new]} (peek (:redo b))]
    (let [to (+ from (count old))]
      (-> b
          (assoc :text (gap/edit (:text b) from to new))
          (update :redo pop)
          (update :undo #(cap (conj % {:from from :old new :new old})))
          (assoc :point (+ from (count new)))))
    b))
