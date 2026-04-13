(ns xmas.buf)

(def undo-limit 1000)

(defn make
  ([name] (make name "" nil))
  ([name text file]
   {:name name :text text :point 0 :mark nil :file file
    :modified false :mode :fundamental :undo [] :redo [] :hscroll 0}))

(defn set-point [b f]
  (assoc b :point (-> (f (:text b) (:point b))
                      (max 0) (min (count (:text b))))))

(defn edit [b from to repl]
  (let [t (:text b)
        old (subs t from to)
        delta (- (count repl) (- to from))]
    (-> b
        (assoc :text (str (subs t 0 from) repl (subs t to)))
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
    (let [t (:text b) to (+ from (count new))]
      (-> b
          (assoc :text (str (subs t 0 from) old (subs t to)))
          (update :undo pop)
          (update :redo #(cap (conj % {:from from :old new :new old})))
          (assoc :point (+ from (count old)))))
    b))

(defn redo [b]
  (if-let [{:keys [from old new]} (peek (:redo b))]
    (let [t (:text b) to (+ from (count old))]
      (-> b
          (assoc :text (str (subs t 0 from) new (subs t to)))
          (update :redo pop)
          (update :undo #(cap (conj % {:from from :old old :new new})))
          (assoc :point (+ from (count new)))))
    b))
