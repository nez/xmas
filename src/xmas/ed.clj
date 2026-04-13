(ns xmas.ed
  (:require [clojure.string :as str]
            [xmas.buf :as buf]
            [xmas.term :as t]
            [xmas.text :as text]
            [xmas.view :as view]
            [xmas.log :as log]
            [nrepl.server :as nrepl])
  (:gen-class))

;; --- Editor state ---

(def editor (atom nil))

(defn cur [s] (get (:bufs s) (:buf s)))
(defn update-cur [s f] (update-in s [:bufs (:buf s)] f))
(defn msg [s message] (assoc s :msg message))
(defn set-point [s f] (update-cur s #(buf/set-point % f)))
(defn edit [s from to repl] (update-cur s #(buf/edit % from to repl)))

;; --- Commands (state → state) ---

(defn forward-char [s]  (set-point s (fn [t p] (text/next-pos t p))))
(defn backward-char [s] (set-point s (fn [t p] (text/prev-pos t p))))
(defn beginning-of-line [s] (set-point s text/line-start))
(defn end-of-line [s]       (set-point s text/line-end))
(defn beginning-of-buffer [s] (set-point s (fn [_ _] 0)))
(defn end-of-buffer [s]       (set-point s (fn [t _] (count t))))

(defn next-line [s]
  (set-point s (fn [t p]
    (let [col (text/display-width t (text/line-start t p) p)
          nxt (min (inc (text/line-end t p)) (count t))]
      (if (>= nxt (count t)) (count t)
        (text/pos-at-col t nxt (text/line-end t nxt) col))))))

(defn previous-line [s]
  (set-point s (fn [t p]
    (let [col (text/display-width t (text/line-start t p) p)
          ls (text/line-start t p)]
      (if (<= ls 0) 0
        (let [pe (dec ls)]
          (text/pos-at-col t (text/line-start t pe) pe col)))))))

(defn forward-word [s]  (set-point s text/word-forward))
(defn backward-word [s] (set-point s text/word-backward))

(defn self-insert [s]
  (let [k (:last-key s) p (:point (cur s))]
    (cond (char? k)   (edit s p p (str k))
          (string? k) (edit s p p k)
          :else s)))

(defn insert-newline [s] (let [p (:point (cur s))] (edit s p p "\n")))

(defn delete-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (< p (count t)) (edit s p (text/next-pos t p) "") s)))

(defn delete-backward-char [s]
  (let [b (cur s) p (:point b) t (:text b)]
    (if (> p 0) (edit s (text/prev-pos t p) p "") s)))

(defn kill-line [s]
  (let [b (cur s) p (:point b) t (:text b)
        eol (text/line-end t p)
        end (if (= p eol) (min (inc eol) (count t)) eol)]
    (if (< p end)
      (-> (edit s p end "") (update :kill conj (subs t p end)))
      s)))

(defn kill-region [s]
  (let [b (cur s) mark (:mark b)]
    (if-not mark s
      (let [from (min mark (:point b)) to (max mark (:point b))]
        (-> (edit s from to "")
            (update :kill conj (subs (:text b) from to))
            (update-cur #(assoc % :mark nil)))))))

(defn yank [s]
  (if (seq (:kill s))
    (let [p (:point (cur s))] (edit s p p (peek (:kill s))))
    s))

(defn set-mark [s] (update-cur s #(assoc % :mark (:point %))))

(defn undo [s]
  (if (seq (:undo (cur s)))
    (update-cur s buf/undo)
    (msg s "No undo")))

(defn keyboard-quit [s]
  (if (:mini s)
    (-> s (assoc :buf (:prev-buf (:mini s)) :mini nil) (msg "Quit"))
    (-> s (update-cur #(assoc % :mark nil)) (msg "Quit"))))

(defn save-buffer [s]
  (let [b (cur s)]
    (if-let [f (:file b)]
      (do (spit f (:text b))
          (-> s (update-cur #(assoc % :modified false)) (msg (str "Wrote " f))))
      (msg s "No file"))))

(defn find-file [s filename]
  (let [content (try (slurp filename) (catch Exception _ ""))
        name (or (last (str/split filename #"/")) filename)]
    (-> s (assoc-in [:bufs name] (buf/make name content filename)) (assoc :buf name))))

(defn switch-buffer [s name]
  (if (get (:bufs s) name)
    (assoc s :buf name)
    (-> s (assoc-in [:bufs name] (buf/make name)) (assoc :buf name))))

;; --- Minibuffer ---

(defn mini-start [s prompt on-done]
  (let [mb " *mini*"]
    (-> s
        (assoc-in [:bufs mb] (buf/make mb))
        (assoc :mini {:prompt prompt :on-done on-done :prev-buf (:buf s)})
        (assoc :buf mb))))

(defn mini-accept [s]
  (if-let [{:keys [on-done prev-buf]} (:mini s)]
    (let [input (:text (cur s))]
      (-> s
          (assoc :buf prev-buf :mini nil)
          (on-done input)))
    s))

;; --- Bindings ---

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
      (and (:mini s) (= key :return))
      (mini-accept s)

      :else
      (let [binding (get bindings key)]
        (cond
          (map? binding) (let [k2 (t/read-key) c (get binding k2)]
                           (if c (let [r (c s)] (if (= r :exit) :exit r))
                             (msg s (str "C-x " k2 " undefined"))))
          (fn? binding)  (binding s)
          (char? key)    (if (>= (int key) 32) (self-insert s) s)
          (string? key)  (self-insert s)
          :else          (msg s (str key " undefined")))))))

;; --- Main ---

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

(defn -main [& _args]
  (log/init!) (log/log "xmas starting")
  (try
    (t/enter-raw-mode!) (t/cls)
    (let [[rows cols] (t/terminal-size)]
      (reset! editor
        {:buf "*scratch*"
         :bufs {"*scratch*" (buf/make "*scratch*"
                  ";; xmas\n;; C-f/b/n/p move, C-x C-f open, C-x C-c quit\n\n" nil)}
         :kill [] :msg nil :mini nil
         :scroll 0 :rows rows :cols cols :last-key nil})
      (when-let [f (let [p (str (System/getProperty "user.home") "/.xmas/init.clj")]
                     (when (.exists (java.io.File. p)) p))]
        (try (load-file f)
             (catch Exception e (swap! editor msg (str "Init: " (.getMessage e))))))
      ;; Redirect stdout/stderr so nREPL output doesn't corrupt the terminal
      (let [null-out (java.io.PrintStream. (java.io.OutputStream/nullOutputStream))]
        (System/setOut null-out)
        (System/setErr null-out))
      (let [srv (nrepl/start-server :port 7888)]
        (log/log "nrepl on port 7888")
        (spit ".nrepl-port" "7888")
        (try (command-loop)
          (finally (nrepl/stop-server srv)))))
    (finally
      (log/log "xmas exiting") (log/close!)
      (t/reset-sg) (t/cls) (t/move 0 0) (t/show-cur) (t/flush!)
      (t/exit-raw-mode!))))
