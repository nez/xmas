(ns xmas.web
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [org.httpkit.server :as http]
            [xmas.term :as t]
            [xmas.view :as view]
            [xmas.log :as log]))

(defonce clients (atom #{}))

(defn render-to-string [s]
  (let [sb (StringBuilder.)]
    (binding [t/*out-fn* (fn [x] (.append sb (str x)))]
      (view/render (dissoc s :render-sig)))
    (str sb)))

(def ^:private broadcast-keys
  ;; Any key here triggers a re-render for web clients. Window-tree and
  ;; query-replace changes used to be missed — splitting windows or
  ;; entering query-replace wouldn't redraw until some unrelated key
  ;; touched one of the watched slots.
  [:bufs :buf :mini :msg :isearch :windows :cur-window :query-replace])

(defn broadcast! [_ _ old new]
  (when (some #(not= (% old) (% new)) broadcast-keys)
    (let [frame (render-to-string new)]
      (doseq [ch @clients]
        (try (http/send! ch frame)
             (catch Exception _ (swap! clients disj ch)))))))

(defn- ws-handler [editor-atom handle-key-fn req]
  (http/as-channel req
    {:on-open (fn [ch]
       (swap! clients conj ch)
       (log/log "web: client connected")
       (http/send! ch (render-to-string @editor-atom)))
     :on-close (fn [ch _]
       (swap! clients disj ch)
       (log/log "web: client disconnected"))
     :on-receive (fn [_ch msg]
       (try
         (let [key (edn/read-string msg)]
           (swap! editor-atom #(handle-key-fn % key)))
         (catch Exception e
           (log/log "web: bad key" (.getMessage e)))))}))

(defn- handler [editor-atom handle-key-fn]
  (fn [req]
    (if (= (:uri req) "/ws")
      (ws-handler editor-atom handle-key-fn req)
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body (slurp (io/resource "xmas/client.html"))})))

(defn start! [editor-atom port handle-key-fn]
  (add-watch editor-atom :web broadcast!)
  (let [srv (http/run-server (handler editor-atom handle-key-fn) {:port port})]
    (log/log "web: listening on" port)
    srv))

(defn stop! [srv editor-atom]
  (srv)
  (remove-watch editor-atom :web)
  (reset! clients #{}))
