(ns xmas.face
  "Per-mode tokenizer. tokenize returns [spans end-state] so callers can
   thread state across lines (e.g. to handle multi-line strings).")

(def ^:private clj-specials
  #{"defn" "defn-" "def" "defmacro" "defmulti" "defmethod" "defprotocol"
    "defrecord" "deftype" "defonce" "defspec" "deftest"
    "fn" "let" "let*" "letfn" "loop" "recur" "if" "if-let" "if-not"
    "when" "when-let" "when-not" "cond" "condp" "case" "do" "for" "doseq" "dotimes"
    "try" "catch" "finally" "throw" "and" "or" "not"
    "quote" "ns" "require" "import" "use" "in-ns"
    "->" "->>" "some->" "some->>" "as->" "cond->" "cond->>"})

(def ^:private el-specials
  #{"defun" "defvar" "defconst" "defmacro" "defsubst" "defcustom" "defgroup"
    "lambda" "let" "let*" "progn" "prog1" "prog2" "if" "when" "unless" "cond"
    "while" "and" "or" "not" "quote" "setq" "setq-default"
    "save-excursion" "save-restriction" "condition-case"
    "define-key" "define-derived-mode" "define-minor-mode"
    "add-hook" "run-hooks"})

(defmulti tokenize
  "Return [spans end-state]. `state` is :normal or :in-string (or mode-specific)."
  (fn [mode _line _state] mode))

(defmethod tokenize :default [_ line _]
  [(if (zero? (count line)) [] [[0 (count line) :default]]) :normal])

;; --- Lisp tokenizer ---

(def ^:private delim #{\( \) \[ \] \{ \} \' \` \, \@ \" \;})

(defn- sym-end ^long [^String line ^long i]
  (let [n (.length line)]
    (loop [i i]
      (if (or (>= i n)
              (Character/isWhitespace (.charAt line i))
              (contains? delim (.charAt line i)))
        i (recur (inc i))))))

(defn- scan-string-close
  "Starting at `i`, scan forward for an unescaped closing quote. Return the
   index of the closing quote, or nil if unterminated."
  [^String line ^long i]
  (let [n (.length line)]
    (loop [i i]
      (cond
        (>= i n)                nil
        (= (.charAt line i) \\) (recur (min n (+ i 2)))
        (= (.charAt line i) \") i
        :else                   (recur (inc i))))))

(defn- emit
  "Append [from to face], merging with adjacent same-face spans."
  [acc from to face]
  (if (>= from to)
    acc
    (let [li (dec (count acc))
          [_ lt lf] (when (nat-int? li) (nth acc li nil))]
      (if (and lt (= lt from) (= lf face))
        (assoc-in acc [li 1] to)
        (conj acc [from to face])))))

(defn- lisp-normal
  "Tokenize `line` starting at `start` in :normal state. Returns [spans end-state]."
  [^String line specials ^long start]
  (let [n (.length line)]
    (loop [i start acc [] prev start]
      (if (>= i n)
        [(emit acc prev n :default) :normal]
        (let [c (.charAt line i)]
          (cond
            (= c \;)
            [(-> acc (emit prev i :default) (emit i n :comment)) :normal]

            (= c \")
            (if-let [close (scan-string-close line (inc i))]
              (recur (inc close)
                     (-> acc (emit prev i :default) (emit i (inc close) :string))
                     (inc close))
              [(-> acc (emit prev i :default) (emit i n :string)) :in-string])

            (= c \:)
            (let [end (sym-end line (inc i))]
              (if (> end (inc i))
                (recur end (-> acc (emit prev i :default) (emit i end :keyword)) end)
                (recur (inc i) acc prev)))

            (and (not (Character/isWhitespace c))
                 (not (contains? delim c)))
            (let [end (sym-end line i)]
              (if (and (> end i)
                       (contains? specials (.substring line i end)))
                (recur end (-> acc (emit prev i :default) (emit i end :builtin)) end)
                (recur end acc prev)))

            :else
            (recur (inc i) acc prev)))))))

(defn- lisp-tokens
  [^String line specials start-state]
  (let [n (.length line)]
    (case start-state
      :in-string
      (if-let [close (scan-string-close line 0)]
        (let [[rest end-state] (lisp-normal line specials (inc close))]
          [(vec (cons [0 (inc close) :string] rest)) end-state])
        [[[0 n :string]] :in-string])
      ;; Default (incl. :normal or any alien state carried over from a
      ;; different mode's line-states cache): tokenize from scratch.
      (lisp-normal line specials 0))))

(defmethod tokenize :clojure-mode [_ line state]
  (lisp-tokens line clj-specials (or state :normal)))

(defmethod tokenize :emacs-lisp-mode [_ line state]
  (lisp-tokens line el-specials (or state :normal)))
