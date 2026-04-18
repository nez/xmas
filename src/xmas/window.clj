(ns xmas.window
  "Window tree + layout. A window is either a leaf (viewport showing one
   buffer) or a split (dir = :stacked for C-x 2 or :side-by-side for C-x 3).
   Paths into the tree are vectors of :a / :b keys; the root is `[]`.")

(defn leaf
  ([buf-name] (leaf buf-name 0 0))
  ([buf-name scroll hscroll]
   {:type :leaf :buffer buf-name :scroll scroll :hscroll hscroll}))

(defn leaf? [w] (= :leaf (:type w)))

(defn- split-node [dir a b]
  {:type :split :dir dir :a a :b b :ratio 0.5})

(defn split
  "Split the leaf at `path` into two windows arranged by `dir`. `:a` inherits
   the original leaf's state; `:b` is a sibling with the same buffer/scroll
   but no saved point, so it will use the buffer's live point the first time
   it gains focus."
  [tree path dir]
  (let [l (get-in tree path)]
    (if (leaf? l)
      (let [sibling (dissoc l :point :mark)
            new-node (split-node dir l sibling)
            new-path (conj (vec path) :a)]
        [(if (seq path) (assoc-in tree path new-node) new-node)
         new-path])
      [tree path])))

;; --- Size adjustment ---

(defn- nearest-ancestor
  "Return [ancestor-path side] of the nearest split with `dir` above `path`,
   or nil if none exists."
  [tree path dir]
  (loop [path (vec path)]
    (when (seq path)
      (let [parent-path (pop path)
            parent (get-in tree parent-path)]
        (if (and (map? parent) (= :split (:type parent)) (= dir (:dir parent)))
          [parent-path (peek path)]
          (recur parent-path))))))

(defn adjust-size
  "Resize the current window along `dir` by `delta` of `total` (rows for
   :stacked, cols for :side-by-side). Returns new tree."
  [tree path dir total delta]
  (if-let [[ancestor-path side] (nearest-ancestor tree path dir)]
    (let [split (get-in tree ancestor-path)
          ratio (:ratio split 0.5)
          step  (/ (double delta) (max 1 (double total)))
          min-r 0.1 max-r 0.9
          new-ratio (cond-> (if (= side :a) (+ ratio step) (- ratio step))
                      true (max min-r)
                      true (min max-r))]
      (if (seq ancestor-path)
        (assoc-in tree (conj ancestor-path :ratio) new-ratio)
        (assoc tree :ratio new-ratio)))
    tree))

(defn leaves
  "Return a vector of paths (vectors of :a/:b) for every leaf in the tree
   in pre-order (left-first)."
  [tree]
  (letfn [(walk [node path acc]
            (if (leaf? node)
              (conj acc path)
              (->> acc
                   (walk (:a node) (conj path :a))
                   (walk (:b node) (conj path :b)))))]
    (walk tree [] [])))

(defn next-leaf
  "Return the path of the next leaf after `path` in pre-order, cycling."
  [tree path]
  (let [all (leaves tree)
        i   (.indexOf ^java.util.List all path)]
    (nth all (mod (inc i) (count all)))))

(defn prev-leaf
  [tree path]
  (let [all (leaves tree)
        i   (.indexOf ^java.util.List all path)]
    (nth all (mod (dec i) (count all)))))

(defn delete-window
  "Remove the leaf at `path` from `tree`, promoting its sibling. Returns
   [new-tree new-path]. Returns [tree path] when there is only one window."
  [tree path]
  (if (empty? path)
    [tree path]
    (let [parent-path (pop (vec path))
          side        (peek (vec path))
          sibling     (get-in tree (conj parent-path (if (= side :a) :b :a)))
          new-tree    (if (seq parent-path)
                        (assoc-in tree parent-path sibling)
                        sibling)
          ;; pick first leaf under the promoted sibling
          new-path (if (leaf? sibling)
                     parent-path
                     (into (vec parent-path) (first (leaves sibling))))]
      [new-tree new-path])))

(defn only
  "Collapse the tree to just the leaf at `path`."
  [tree path]
  [(get-in tree path) []])

(defn replace-buffer
  "Rewrite every leaf whose :buffer is `old` to show `new` instead, dropping
   any saved point/mark so stale coords from the old buffer can't leak in."
  [tree old new]
  (cond
    (leaf? tree)
    (if (= (:buffer tree) old)
      (-> tree (assoc :buffer new :scroll 0 :hscroll 0) (dissoc :point :mark))
      tree)
    (= :split (:type tree))
    (-> tree
        (update :a replace-buffer old new)
        (update :b replace-buffer old new))
    :else tree))

;; --- Layout: compute screen rect for each leaf path ---

(defn- split-size [total ratio]
  (let [a (-> (int (* (double total) (double ratio))) (max 1) (min (- total 2)))
        b (max 0 (- total a 1))]
    [a b]))

(defn- layout-node [node row col rows cols]
  (if (leaf? node)
    [[[] {:row row :col col :rows rows :cols cols}]]
    (let [dir   (:dir node)
          ratio (:ratio node 0.5)
          [ar ac arows acols br bc brows bcols]
          (case dir
            :stacked
            (let [[top bottom] (split-size rows ratio)]
              [row col top cols (+ row top 1) col bottom cols])
            :side-by-side
            (let [[left right] (split-size cols ratio)]
              [row col rows left row (+ col left 1) rows right]))]
      (concat
        (map (fn [[p r]] [(cons :a p) r]) (layout-node (:a node) ar ac arows acols))
        (map (fn [[p r]] [(cons :b p) r]) (layout-node (:b node) br bc brows bcols))))))

(defn layout
  "Return a map of path → rect {:row :col :rows :cols} for each leaf."
  [tree rows cols]
  (into {}
    (map (fn [[p r]] [(vec p) r])
         (layout-node tree 0 0 rows cols))))

(defn dividers
  "Return a seq of divider segments: {:kind :horizontal|:vertical :row :col :len}."
  [tree rows cols]
  (letfn [(walk [node row col rows cols acc]
            (if (leaf? node)
              acc
              (let [ratio (:ratio node 0.5)]
                (case (:dir node)
                  :stacked
                  (let [[top bottom] (split-size rows ratio)
                        div {:kind :horizontal :row (+ row top) :col col :len cols}
                        acc (conj acc div)
                        acc (walk (:a node) row col top cols acc)]
                    (walk (:b node) (+ row top 1) col bottom cols acc))
                  :side-by-side
                  (let [[left right] (split-size cols ratio)
                        div {:kind :vertical :row row :col (+ col left) :len rows}
                        acc (conj acc div)
                        acc (walk (:a node) row col rows left acc)]
                    (walk (:b node) row (+ col left 1) rows right acc))))))]
    (walk tree 0 0 rows cols [])))
