(ns forest-run.ui
  (:require [forest-run.impi]
            [impi.core :as impi]
            [forest-run.core :as core]
            [forest-run.animate :as animate]
            [forest-run.events :as events]
            [forest-run.ui-state-utils :as utils]
            [forest-run.views :as views]
            [forest-run.viz-events :as viz-events]
            [cljs.core.async :as async]))

(enable-console-print!)

(defonce state (atom nil))
(defonce msg-chan (async/chan))

(defn init-stage! []
  (reset!
   state
   (let [game-state (core/init-game-state)]
     {:game-state game-state

      :game #:game {:segment-index 0}

      :canvas
      #:canvas {:color 0xd7eff1 #_0x0a1c5e}
      :player
      #:player {:selected? false
                :pos       (utils/card-pos (-> game-state last :position))
                :pulse     0.0}
      :field
      #:field  {:cards (->> (for [[r-idx row] (map-indexed vector (-> game-state last :deck))]
                              (for [[c-idx card] (map-indexed vector row)]
                                [[c-idx r-idx] {:flipped 180}]))
                            (apply concat)
                            (into {}))
                :y     (- (* (+ utils/card-h utils/card-spacing) 3)
                          (/ utils/card-h 2))}}))
  (async/put! msg-chan {:type   :event
                        :key    :player/pulse
                        :parent {:type :start
                                 :key  :game/start}
                        :args   {:duration 120}}))

(defn compact-events [events-map {:keys [parent] :as event}]
  (update events-map
          (select-keys event [:type :key])
          #(into (set %1) (if %2 [%2] []))
          (:key parent)))

(defonce events-map (atom {}))

(comment
  (viz-events/graph-events! @events-map))

(defn take-all! [chan msg-keys]
  (loop [elements (zipmap msg-keys (repeat []))]
    (if-let [element (async/poll! chan)]
      (do
        (swap! events-map compact-events element)
        (if (contains? (set msg-keys) (:type element))
          (recur (update elements (:type element) conj element))))
      elements)))

(defn put-all! [chan msgs parent]
  (doseq [msg msgs]
    (async/put! chan (assoc msg :parent (dissoc parent :parent)))))

(defn game-handler [delta-time]

  ;; process all the new events
  ;; may generate updates/animations/events

  (let [{events     :event
         updates    :update
         animations :animation} (take-all! msg-chan [:event
                                                     :update
                                                     :animation])]

    (doseq [e events]
      (put-all! msg-chan (events/handler-event e) e))

    ;; process any animations
    (doseq [a animations]
      (put-all! msg-chan (animate/apply-animation delta-time a) a)

      (when (and (:post-steps a) (not (:steps a)))
        (put-all! msg-chan ((:post-steps a) @state) a)))

    ;; process all the updates
    (when (seq updates)
      (let [[old new] (swap-vals! state (apply comp (map :update-fn updates)))]
        (doseq [update updates]
          (when (:reaction update)
            (put-all! msg-chan ((:reaction update) old new) update)))))))

(defonce ticker (atom nil))

(defn start-ticker! []
  (reset! ticker
          (doto (js/PIXI.ticker.Ticker.)
            (.stop)
            (.add game-handler)
            (.start))))

(defn destory-ticker! []
  (if-let [ticker' @ticker]
    (do (doto ticker'
          (.stop)
          (.destroy))
        (reset! ticker nil))))

(def render-state (partial views/render-state msg-chan))

(defn start-renderer! []
  (let [element (.getElementById js/document "app")]
    (when @state (impi/mount :game (render-state @state) element))
    (add-watch state ::renderer (fn [_ _ _ s]
                                  (impi/mount :game (render-state s) element)))))

(defn stop-renderer! []
  (remove-watch state ::renderer))

(defn start []
  (init-stage!)
  (start-renderer!)
  (start-ticker!))

(defn pause []
  (if-let [ticker @ticker]
    (.stop ticker)))

(defn continue []
  (if-let [ticker @ticker]
    (.start ticker)))

(defn stop []
  (stop-renderer!)
  (destory-ticker!))

(defn reset []
  (stop)
  (start))

(comment
  (reset))
