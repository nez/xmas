(ns xmas.mode
  "Major and minor mode registry. Lives as data on the editor state under
   :modes {name {:type :major|:minor :keymap {...}}} and :hooks {name [fns]}."
  (:require [xmas.cmd :as cmd]))

;; --- Registration ---

(defn register
  [s mode-name type & {:keys [keymap]}]
  (-> s
      (assoc-in [:modes mode-name :type] type)
      (update-in [:modes mode-name :keymap] #(merge (or % {}) keymap))))

;; --- Hooks ---

(defn add-hook [s hook-name f]
  (update-in s [:hooks hook-name] (fnil conj []) f))

(defn run-hooks [s hook-name]
  (reduce (fn [s f] (f s)) s (get-in s [:hooks hook-name] [])))

;; --- Activation ---

(defn- hook-name-for [mode-name]
  (symbol (str (name mode-name) "-hook")))

(defn set-major-mode [s mode-name]
  (-> s
      (cmd/update-cur #(assoc % :mode mode-name :line-states []))
      (run-hooks (hook-name-for mode-name))))

(defn toggle-minor-mode [s mode-name]
  (let [active (vec (:minor-modes (cmd/cur s)))
        active? (some #{mode-name} active)
        next-active (if active? (filterv #(not= mode-name %) active)
                                (conj active mode-name))]
    (-> s
        (cmd/update-cur #(assoc % :minor-modes next-active))
        (run-hooks (hook-name-for mode-name)))))

;; --- Key lookup ---
;; Active minor modes override the major mode, which overrides global.

(defn lookup-key [s key]
  (let [b (cmd/cur s)]
    (or (some (fn [m] (get-in s [:modes m :keymap key]))
              (rseq (vec (:minor-modes b))))
        (get-in s [:modes (:mode b) :keymap key]))))

;; --- File extension → mode ---

(def ^:private ext->mode
  {".clj" :clojure-mode ".cljc" :clojure-mode ".cljs" :clojure-mode
   ".edn" :clojure-mode
   ".el"  :emacs-lisp-mode})

(defn mode-for-file
  "Return the major-mode symbol matching the filename's extension, or nil."
  [^String filename]
  (when filename
    (let [dot (.lastIndexOf filename ".")]
      (when (pos? dot)
        (get ext->mode (.substring filename dot))))))
