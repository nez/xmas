(ns xmas.ed
  (:require [xmas.term :as t]
            [xmas.view :as view]
            [xmas.log :as log])
  (:gen-class))

;; ============================================================================
;; State
;; ============================================================================

(def editor (atom nil))

(defn buf [s] (get (:bufs s) (:buf s)))
(defn update-buf [s f] (update-in s [:bufs (:buf s)] f))
(defn msg [s text] (assoc s :msg text))

(defn make-buf
  ([name] (make-buf name "" nil))
  ([name text file]
   {:text text :point 0 :mark nil :file file
    :modified false :mode :fundamental :undo []}))

(defn set-point [s f]
  (update-buf s (fn [b]
    (assoc b :point (-> (f (:text b) (:point b))
                        (max 0) (min (count (:text b))))))))

;; ============================================================================
;; One edit primitive: replace text[from..to] with repl
;; ============================================================================

(defn edit [s from to repl]
  (update-buf s (fn [b]
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
                  :else      (+ p delta)))))))))

(defn edit-undo [s]
  (if-let [{:keys [from old new]} (first (:undo (buf s)))]
    (update-buf s (fn [b]
      (let [t (:text b) to (+ from (count new))]
        (-> b
            (assoc :text (str (subs t 0 from) old (subs t to)))
            (update :undo rest)
            (assoc :point (+ from (count old)))))))
    (msg s "No undo")))

;; ============================================================================
;; Commands (state → state)
;; ============================================================================

(defn forward-char [s]  (set-point s (fn [_ p] (inc p))))
(defn backward-char [s] (set-point s (fn [_ p] (dec p))))
(defn beginning-of-line [s] (set-point s view/line-start))
(defn end-of-line [s]       (set-point s view/line-end))
(defn beginning-of-buffer [s] (set-point s (fn [_ _] 0)))
(defn end-of-buffer [s]       (set-point s (fn [t _] (count t))))

(defn next-line [s]
  (update-buf s (fn [b]
    (let [t (:text b) p (:point b)
          col (- p (view/line-start t p))
          nxt (min (inc (view/line-end t p)) (count t))]
      (assoc b :point (if (>= nxt (count t)) (count t)
                        (min (+ nxt col) (view/line-end t nxt))))))))

(defn previous-line [s]
  (update-buf s (fn [b]
    (let [t (:text b) p (:point b)
          col (- p (view/line-start t p))
          ls (view/line-start t p)]
      (if (<= ls 0) (assoc b :point 0)
        (let [pe (dec ls)]
          (assoc b :point (min (+ (view/line-start t pe) col) pe))))))))

(defn- scan-word [t start dir]
  (loop [p start in false]
    (if (if (pos? dir) (>= p (count t)) (< p 0))
      (if (pos? dir) (count t) 0)
      (let [wc (Character/isLetterOrDigit (.charAt t (int p)))]
        (if (and in (not wc))
          (if (pos? dir) p (inc p))
          (recur (+ p dir) wc))))))

(defn forward-word [s]  (set-point s (fn [t p] (scan-word t p 1))))
(defn backward-word [s] (set-point s (fn [t p] (scan-word t (dec p) -1))))

(defn self-insert [s]
  (let [k (:last-key s)]
    (if (char? k) (edit s (:point (buf s)) (:point (buf s)) (str k)) s)))

(defn insert-newline [s] (edit s (:point (buf s)) (:point (buf s)) "\n"))

(defn delete-char [s]
  (let [p (:point (buf s))]
    (if (< p (count (:text (buf s)))) (edit s p (inc p) "") s)))

(defn delete-backward-char [s]
  (let [p (:point (buf s))]
    (if (> p 0) (edit s (dec p) p "") s)))

(defn kill-line [s]
  (let [b (buf s) p (:point b) t (:text b)
        eol (view/line-end t p)
        end (if (= p eol) (min (inc eol) (count t)) eol)]
    (if (< p end)
      (-> (edit s p end "") (update :kill conj (subs t p end)))
      s)))

(defn kill-region [s]
  (let [b (buf s) mark (:mark b)]
    (if-not mark s
      (let [from (min mark (:point b)) to (max mark (:point b))]
        (-> (edit s from to "")
            (update :kill conj (subs (:text b) from to))
            (update-buf #(assoc % :mark nil)))))))

(defn yank [s]
  (if (seq (:kill s))
    (let [p (:point (buf s))] (edit s p p (peek (:kill s))))
    s))

(defn set-mark [s] (update-buf s #(assoc % :mark (:point %))))
(defn undo [s] (edit-undo s))
(defn keyboard-quit [s]
  (if (:mini s)
    ;; abort minibuffer — restore previous buffer
    (-> s (assoc :buf (:prev-buf (:mini s)) :mini nil) (msg "Quit"))
    (-> s (update-buf #(assoc % :mark nil)) (msg "Quit"))))

(defn save-buffer [s]
  (let [b (buf s)]
    (if-let [f (:file b)]
      (do (spit f (:text b))
          (-> s (update-buf #(assoc % :modified false)) (msg (str "Wrote " f))))
      (msg s "No file"))))

(defn find-file [s filename]
  (let [text (try (slurp filename) (catch Exception _ ""))
        name (or (last (clojure.string/split filename #"/")) filename)]
    (-> s (assoc-in [:bufs name] (make-buf name text filename)) (assoc :buf name))))

(defn switch-buffer [s name]
  (if (get (:bufs s) name)
    (assoc s :buf name)
    (-> s (assoc-in [:bufs name] (make-buf name)) (assoc :buf name))))

;; ============================================================================
;; Minibuffer — activates a real buffer, normal commands just work
;; ============================================================================

(defn mini-start [s prompt on-done]
  (let [mb " *mini*"]
    (-> s
        (assoc-in [:bufs mb] (make-buf mb))
        (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)})
        (assoc :buf mb))))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input (:text (buf s))]
      (-> s
          (assoc :buf prev-buf :mini nil)
          (on-done input)))
    s))

;; ============================================================================
;; Bindings — nested maps = prefixes, fns = commands
;; ============================================================================

(def bindings
  {[:ctrl \f] forward-char,   [:ctrl \b] backward-char
   [:ctrl \n] next-line,      [:ctrl \p] previous-line
   [:ctrl \a] beginning-of-line, [:ctrl \e] end-of-line
   [:meta \f] forward-word,   [:meta \b] backward-word
   [:meta \<] beginning-of-buffer, [:meta \>] end-of-buffer
   :up previous-line, :down next-line, :right forward-char, :left backward-char
   :home beginning-of-line, :end end-of-line
   [:ctrl \d] delete-char, :backspace delete-backward-char
   [:ctrl \k] kill-line, [:ctrl \w] kill-region
   [:ctrl \y] yank, [:ctrl \space] set-mark
   [:ctrl \/] undo, [:ctrl \g] keyboard-quit
   [:ctrl \x] {[:ctrl \s] save-buffer
               [:ctrl \f] (fn [s] (mini-start s "Find file: " find-file))
               [:ctrl \c] (fn [_] :exit)
               \b         (fn [s] (mini-start s "Buffer: " switch-buffer))}})

(defn handle-key [s key]
  (let [s (assoc s :last-key key)]
    (cond
      ;; in minibuffer: RET accepts, C-g quits (handled above), rest are normal cmds
      (and (:mini s) (= key :return))
      (mini-accept s)

      ;; normal dispatch
      :else
      (let [b (get bindings key)]
        (cond
          (map? b)    (let [k2 (t/read-key) c (get b k2)]
                        (if c (let [r (c s)] (if (= r :exit) :exit r))
                          (msg s (str "C-x " k2 " undefined"))))
          (fn? b)     (b s)
          (char? key) (if (>= (int key) 32) (self-insert s) s)
          :else       (msg s (str key " undefined")))))))

;; ============================================================================
;; Loop + main
;; ============================================================================

(defn command-loop []
  (loop []
    (let [s @editor
          s (assoc s :scroll (view/render s))]
      (reset! editor s)
      (when-let [key (t/read-key)]
        (let [r (handle-key s key)]
          (when (not= r :exit)
            (reset! editor r)
            (recur)))))))

(defn -main [& args]
  (log/init!) (log/log "xmas starting")
  (try
    (t/enter-raw-mode!) (t/cls)
    (let [[rows cols] (t/terminal-size)]
      (reset! editor
        {:buf "*scratch*"
         :bufs {"*scratch*" (make-buf "*scratch*"
                  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n" nil)}
         :kill [] :msg nil :mini nil
         :scroll 0 :rows rows :cols cols :last-key nil})
      (when-let [f (let [p (str (System/getProperty "user.home") "/.xmas/init.clj")]
                     (when (.exists (java.io.File. p)) p))]
        (try (load-file f)
             (catch Exception e (swap! editor msg (str "Init: " (.getMessage e))))))
      (command-loop))
    (finally
      (log/log "xmas exiting") (log/close!)
      (t/reset-sg) (t/cls) (t/move 0 0) (t/show-cur) (t/flush!)
      (t/exit-raw-mode!))))
