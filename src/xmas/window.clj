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
          new-ratio (-> (if (= side :a) (+ ratio step) (- ratio step))
                        (max 0.1) (min 0.9))]
      (assoc-in tree (conj ancestor-path :ratio) new-ratio))
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

(defn- neighbor-leaf [tree path step]
  (let [all (leaves tree)
        i   (.indexOf ^java.util.List all path)]
    (nth all (mod (step i) (count all)))))

(defn next-leaf
  "Return the path of the next leaf after `path` in pre-order, cycling."
  [tree path] (neighbor-leaf tree path inc))

(defn delete-window
  "Remove the leaf at `path` from `tree`, promoting its sibling. Returns
   [new-tree new-path]. Returns [tree path] when there is only one window."
  [tree path]
  (if (empty? path)
    [tree path]
    (let [path        (vec path)
          parent-path (pop path)
          side        (peek path)
          sibling     (get-in tree (conj parent-path (if (= side :a) :b :a)))
          new-tree    (if (seq parent-path)
                        (assoc-in tree parent-path sibling)
                        sibling)
          ;; pick first leaf under the promoted sibling
          new-path    (if (leaf? sibling)
                        parent-path
                        (into parent-path (first (leaves sibling))))]
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

(defn- split-size
  "Divide `total` rows/cols between two panes plus a 1-unit divider. Clamps
   both panes to >= 0 — otherwise total < 3 produced a negative pane and
   downstream layout walked off the end."
  [total ratio]
  (if (< total 3)
    [(max 0 (quot total 2)) (max 0 (- total 1 (quot total 2)))]
    (let [a (-> (int (* (double total) (double ratio))) (max 1) (min (- total 2)))
          b (- total a 1)]
      [a b])))

(defn geometry
  "Return {:layout {path rect} :dividers [segments]} for a window tree."
  [tree rows cols]
  (letfn [(walk [node path row col rows cols layout dividers]
            (if (leaf? node)
              [(assoc layout path {:row row :col col :rows rows :cols cols}) dividers]
              (let [ratio (:ratio node 0.5)]
                (case (:dir node)
                  :stacked
                  (let [[top bottom] (split-size rows ratio)
                        div {:kind :horizontal :row (+ row top) :col col :len cols}
                        [layout dividers] (walk (:a node) (conj path :a)
                                                row col top cols layout (conj dividers div))]
                    (walk (:b node) (conj path :b) (+ row top 1) col bottom cols
                          layout dividers))
                  :side-by-side
                  (let [[left right] (split-size cols ratio)
                        div {:kind :vertical :row row :col (+ col left) :len rows}
                        [layout dividers] (walk (:a node) (conj path :a)
                                                row col rows left layout (conj dividers div))]
                    (walk (:b node) (conj path :b) row (+ col left 1) rows right
                          layout dividers))))))]
    (let [[layout dividers] (walk tree [] 0 0 rows cols {} [])]
      {:layout layout :dividers dividers})))

(defn layout [tree rows cols] (:layout (geometry tree rows cols)))
(defn dividers [tree rows cols] (:dividers (geometry tree rows cols)))
