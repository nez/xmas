(ns xmas.buf)

(def undo-limit 1000)

(defn bounded-conj
  "Append item to vector v, dropping the oldest if count exceeds limit."
  [v limit item]
  (let [v' (conj v item)]
    (if (> (count v') limit) (subvec v' 1) v')))

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
        (update :undo bounded-conj undo-limit {:from from :old old :new repl})
        (assoc :redo [])
        (update :point (fn [p]
          (cond (< p from) p
                (<= p to)  (+ from (count repl))
                :else      (+ p delta)))))))

(defn- replay
  "Apply the top entry from source stack, push it to target stack.
   undo: removes :new text, restores :old. redo: removes :old, applies :new."
  [b from-key to-key]
  (if-let [{:keys [from old new] :as entry} (peek (from-key b))]
    (let [[remove apply-text] (if (= from-key :undo) [new old] [old new])
          t (:text b)
          to (+ from (count remove))]
      (-> b
          (assoc :text (str (subs t 0 from) apply-text (subs t to)))
          (update from-key pop)
          (update to-key bounded-conj undo-limit entry)
          (assoc :point (+ from (count apply-text)))))
    b))

(defn undo [b] (replay b :undo :redo))
(defn redo [b] (replay b :redo :undo))
