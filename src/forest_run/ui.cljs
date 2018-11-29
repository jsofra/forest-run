(ns forest-run.ui
  (:require [forest-run.impi]
            [impi.core :as impi]
            [forest-run.core :as core]
            [forest-run.animate :as animate]
            [forest-run.events :as events]
            [forest-run.ui-state-utils :as utils]
            [forest-run.views :as views]
            [cljs.core.async :as async]))

(enable-console-print!)

(defonce state (atom nil))
(defonce msg-chan (async/chan))

(defonce msg-pub (async/pub msg-chan :msg/type))

(defonce events-chan (async/chan))
(async/sub msg-pub :event events-chan)
(defonce updates-chan (async/chan))
(async/sub msg-pub :update updates-chan)
(defonce animations-chan (async/chan))
(async/sub msg-pub :animation animations-chan)


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
  (async/put! msg-chan #:event{:msg/type :event
                               :key      :player/pulse
                               :args     {:duration 120}}))

(defn take-all! [chan]
  (loop [elements []]
    (if-let [element (async/poll! chan)]
      (do
        ;(when (= (:msg/type element) :event) (prn :e (:event/key element)))
        (recur (conj elements element)))
      elements)))

(defn game-handler [delta-time]

  ;; process all the new events
  ;; may generate updates/animations/events
  (doseq [e (take-all! events-chan)]
    (events/handler-event msg-chan e))

  (let [updates           (take-all! updates-chan)

        animations        (take-all! animations-chan)
        new-animations    (mapv (partial animate/apply-animation delta-time)
                                animations)

        all-updates       (seq (concat updates (map :update new-animations)))]

    ;; process any animations
    (doseq [a new-animations]
      (when (or (seq (:children a)) (seq (dissoc a :update :children)))
        (prn :a a)
        (async/put! msg-chan a))

      (when (and (:post-steps a) (not (:steps a)))
        ((:post-steps a) msg-chan @state)))

    ;; process all the updates
    (when all-updates
      (let [[old new] (swap-vals! state (apply comp (map :update-fn all-updates)))]
        (doseq [reaction (map :reaction all-updates)]
          (when reaction
            (reaction msg-chan old new)))))))

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
