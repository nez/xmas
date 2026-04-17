(ns xmas.gap)

(def ^:const GAP 128)

;; --- GapBuffer deftype ---
;; Persistent (immutable) gap buffer backed by a char[].
;; Each edit allocates a new array with the gap at the edit point.
;; Implements CharSequence for seamless interop with text.clj.
;;
;; Layout:  [content-before-gap][GAP][content-after-gap]
;;           0..gs               gs..ge             ge..buf.length
;;
;; lidx: sorted int[] of logical newline positions, built on construction.
;;       Enables O(log n) line-start/line-end/line-of lookups.

(deftype GapBuffer [^chars buf ^int gs ^int ge ^ints lidx]
  CharSequence
  (length [_]
    (- (alength buf) (- ge gs)))
  (charAt [_ i]
    (aget buf (if (< i gs) i (+ i (- ge gs)))))
  (subSequence [_ start end]
    (let [gap (- ge gs)
          n   (- end start)]
      (cond
        (<= end gs)   (String. buf (int start) (int n))
        (>= start gs) (String. buf (int (+ start gap)) (int n))
        :else
        (let [sb (StringBuilder. (int n))
              bk (- gs start)]
          (.append sb buf (int start) (int bk))
          (.append sb buf (int ge) (int (- n bk)))
          (.toString sb)))))
  (toString [_]
    (let [sb (StringBuilder. (int (- (alength buf) (- ge gs))))]
      (.append sb buf (int 0) (int gs))
      (.append sb buf (int ge) (int (- (alength buf) ge)))
      (.toString sb)))

  clojure.lang.Counted
  (count [this] (.length this))

  Object
  (equals [this other]
    (cond
      (identical? this other) true
      (nil? other) false
      (instance? CharSequence other)
      (let [^CharSequence o other
            n (.length this)]
        (and (= n (.length o))
             (loop [i 0]
               (if (>= i n) true
                 (if (= (.charAt this (int i)) (.charAt o (int i)))
                   (recur (inc i))
                   false)))))
      :else false))
  (hashCode [this]
    (let [n (.length this)]
      (loop [i 0 h (int 0)]
        (if (>= i n) (int h)
          (recur (inc i)
                 (unchecked-add-int
                   (unchecked-multiply-int h (int 31))
                   (int (.charAt this (int i))))))))))

;; --- Line index ---

(defn- scan-newlines
  "Build sorted int[] of logical newline positions from gap buffer arrays."
  ^ints [^chars buf gs ge]
  (let [gs  (int gs)
        ge  (int ge)
        n   (alength buf)
        gap (- ge gs)
        nl  \newline
        cnt (+ (loop [i 0 c 0]
                 (if (>= i gs) c
                   (recur (inc i) (if (= (aget buf (int i)) nl) (inc c) c))))
               (loop [i ge c 0]
                 (if (>= i n) c
                   (recur (inc i) (if (= (aget buf (int i)) nl) (inc c) c)))))]
    (if (zero? cnt)
      (int-array 0)
      (let [arr (int-array cnt)
            j   (loop [i 0 j 0]
                  (if (>= i gs) j
                    (if (= (aget buf (int i)) nl)
                      (do (aset arr (int j) (int i))
                          (recur (inc i) (inc j)))
                      (recur (inc i) j))))]
        (loop [i ge j j]
          (when (< i n)
            (if (= (aget buf (int i)) nl)
              (do (aset arr (int j) (int (- i gap)))
                  (recur (inc i) (inc j)))
              (recur (inc i) j))))
        arr))))

;; --- Constructors ---

(defn of
  "Create a GapBuffer from a String."
  ^GapBuffer [^String s]
  (let [n   (.length s)
        a   (char-array (+ n (int GAP)))
        gs  (int n)
        ge  (int (+ n (int GAP)))]
    (.getChars s 0 n a 0)
    (GapBuffer. a gs ge (scan-newlines a gs ge))))

;; --- Internal ---

(defn- copy-range!
  "Copy logical [from,to) from a gap buffer's arrays to dest at dest-off."
  [^chars src sgs sge ^chars dst doff from to]
  (let [sgs (long sgs) sge (long sge) doff (long doff)
        from (long from) to (long to) gap (- sge sgs)]
    (cond
      (<= to sgs)
      (System/arraycopy src (int from) dst (int doff) (int (- to from)))
      (>= from sgs)
      (System/arraycopy src (int (+ from gap)) dst (int doff) (int (- to from)))
      :else
      (let [bk (- sgs from)]
        (System/arraycopy src (int from) dst (int doff) (int bk))
        (System/arraycopy src (int sge) dst (int (+ doff bk)) (int (- to sgs)))))))

;; --- Operations ---

(defn edit
  "Replace logical [from,to) with repl. Returns a new GapBuffer
   with the gap positioned right after the inserted text."
  ^GapBuffer [^GapBuffer gb ^long from ^long to ^String repl]
  (let [b   (.-buf gb)
        ogs (long (.-gs gb))
        oge (long (.-ge gb))
        rn  (.length repl)
        cn  (- (alength b) (- oge ogs))
        nn  (+ (- cn (- to from)) rn)
        g   (long (max GAP rn))
        nb  (char-array (+ nn g))
        ngs (int (+ from rn))
        nge (int (+ from rn g))]
    (when (pos? from)
      (copy-range! b ogs oge nb 0 0 from))
    (when (pos? rn)
      (.getChars repl 0 rn nb (int from)))
    (let [rem (- cn to)]
      (when (pos? rem)
        (copy-range! b ogs oge nb (long nge) to cn)))
    (GapBuffer. nb ngs nge (scan-newlines nb ngs nge))))

(defn substr
  "Extract logical [from,to) as a String."
  ^String [^GapBuffer gb ^long from ^long to]
  (.toString (.subSequence gb (int from) (int to))))

;; --- Line index queries ---

(defn line-count
  "Number of lines (newlines + 1)."
  ^long [^GapBuffer gb]
  (inc (alength (.-lidx gb))))

(defn line-of
  "Zero-based line number containing pos. Returns the first lidx index i
   where lidx[i] >= pos (O(log n) via `Arrays/binarySearch`)."
  ^long [^GapBuffer gb ^long pos]
  (let [i (java.util.Arrays/binarySearch (.-lidx gb) (int pos))]
    (if (neg? i) (- (- i) 1) i)))

(defn nth-line-start
  "Start position of zero-based line n."
  ^long [^GapBuffer gb ^long n]
  (if (zero? n) 0
    (inc (aget (.-lidx gb) (int (dec n))))))

(defn nth-line-end
  "End position of zero-based line n (position of \\n, or text length)."
  ^long [^GapBuffer gb ^long n]
  (let [arr (.-lidx gb)
        nl  (alength arr)]
    (if (< n nl)
      (long (aget arr (int n)))
      (long (count gb)))))

(defn line-start
  "Start of line containing logical position pos."
  ^long [^GapBuffer gb ^long pos]
  (nth-line-start gb (line-of gb pos)))

(defn line-end
  "End of line containing logical position pos."
  ^long [^GapBuffer gb ^long pos]
  (nth-line-end gb (line-of gb pos)))
