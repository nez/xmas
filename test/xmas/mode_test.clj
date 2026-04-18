(ns xmas.mode-test
  (:require [clojure.test :refer [deftest is]]
            [xmas.buf :as buf]
            [xmas.mode :as mode]))

(defn- state []
  {:buf "t" :bufs {"t" (buf/make "t")} :msg nil})

(deftest register-and-lookup-major-key
  (let [s (-> (state)
              (mode/register :my-mode :major :keymap {[:ctrl \c] :foo})
              (mode/set-major-mode :my-mode))]
    (is (= :my-mode (:mode (get-in s [:bufs "t"]))))
    (is (= :foo (mode/lookup-key s [:ctrl \c])))))

(deftest define-key-adds-binding
  (let [s (-> (state)
              (mode/register :my-mode :major)
              (mode/set-major-mode :my-mode)
              (mode/define-key :my-mode [:ctrl \x] :bar))]
    (is (= :bar (mode/lookup-key s [:ctrl \x])))))

(deftest minor-mode-overrides-major
  (let [s (-> (state)
              (mode/register :major :major :keymap {[:ctrl \c] :major-cmd})
              (mode/register :minor :minor :keymap {[:ctrl \c] :minor-cmd})
              (mode/set-major-mode :major)
              (mode/toggle-minor-mode :minor))]
    (is (= :minor-cmd (mode/lookup-key s [:ctrl \c])))))

(deftest toggle-minor-mode-removes-it
  (let [s (-> (state)
              (mode/register :m :minor :keymap {[:ctrl \c] :cmd})
              (mode/toggle-minor-mode :m)
              (mode/toggle-minor-mode :m))]
    (is (empty? (:minor-modes (get-in s [:bufs "t"]))))))

(deftest hooks-run-in-order
  (let [tag (atom [])
        hook1 (fn [s] (swap! tag conj :a) s)
        hook2 (fn [s] (swap! tag conj :b) s)]
    (-> (state)
        (mode/add-hook 'my-hook hook1)
        (mode/add-hook 'my-hook hook2)
        (mode/run-hooks 'my-hook))
    (is (= [:a :b] @tag))))

(deftest set-major-mode-runs-mode-hook
  (let [called (atom false)
        hook (fn [s] (reset! called true) s)]
    (-> (state)
        (mode/register :my-mode :major)
        (mode/add-hook 'my-mode-hook hook)
        (mode/set-major-mode :my-mode))
    (is @called)))

(deftest mode-for-file-detects-clojure
  (is (= :clojure-mode (mode/mode-for-file "foo.clj")))
  (is (= :clojure-mode (mode/mode-for-file "bar.cljs")))
  (is (= :emacs-lisp-mode (mode/mode-for-file "init.el")))
  (is (nil? (mode/mode-for-file "README")))
  (is (nil? (mode/mode-for-file nil))))
