(ns xmas.buf)

(defn make
  ([name] (make name "" nil))
  ([name text file]
   {:name name :text text :point 0 :mark nil :file file
    :modified false :mode :fundamental :undo []}))

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
        (update :undo conj {:from from :old old :new repl})
        (update :point (fn [p]
          (cond (< p from) p
                (<= p to)  (+ from (count repl))
                :else      (+ p delta)))))))

(defn undo [b]
  (when-let [{:keys [from old new]} (first (:undo b))]
    (let [t (:text b) to (+ from (count new))]
      (-> b
          (assoc :text (str (subs t 0 from) old (subs t to)))
          (update :undo rest)
          (assoc :point (+ from (count old)))))))
