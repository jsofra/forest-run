(ns forest-run.ui
  (:require [forest-run.impi]
            [impi.core :as impi]
            [forest-run.core :as core]
            [forest-run.animate :as animate]
            [forest-run.events :as events]
            [forest-run.ui-state-utils :as utils]
            [cljs.core.async :as async]))

(enable-console-print!)

(defonce state (atom nil))
(defonce events-chan (async/chan))
(defonce updates-chan (async/chan))
(defonce animations-chan (async/chan))

(defonce channels {:events     events-chan
                   :updates    updates-chan
                   :animations animations-chan})

(defn render-attack
  [card-name attack]
  {:impi/key             (str (name card-name) "-text")
   :pixi.object/type     :pixi.object.type/text
   :pixi.object/position [0 0]
   :pixi.text/anchor     [0.5 0.5]
   :pixi.text/text       attack
   :pixi.text/style
   {:pixi.text.style/align            "left"
    :pixi.text.style/fill             0xff0000
    :pixi.text.style/font-weight      "bold"
    :pixi.text.style/font-family      "Arial"
    :pixi.text.style/font-size        42
    :pixi.text.style/stroke           0x000000
    :pixi.text.style/stroke-thickness 6}})

(def card-defaults
  (merge #:pixi.sprite {:tint     0xFFFFFF
                        :anchor   [0.5 0.5]}
         #:pixi.object {:position [0 0]
                        :rotation 0}))

(defn render-card
  ([{:keys [rank suit flipped]
     :or   {flipped 0}
     :as   card}]
   (let [card-name (keyword (str (name rank) "-" (name suit)))
         flipped   (if (zero? (mod (+ flipped 90) 180)) (+ flipped 0.5) flipped)
         revealed  (odd? (Math/ceil (/ (+ flipped 90) 180)))]
     (merge
      card-defaults
      {:impi/key         card-name
       :pixi.object/type :pixi.object.type/sprite
       :card/flipped     flipped
       :card/revealed    revealed
       :pixi.sprite/texture
       {:pixi.texture/source (if revealed
                               (str "img/" (name card-name) ".png")
                               (str "img/back.png"))}}
      card))))

(defn render-field-card
  [{:keys [attack] :as card} player]
  (let [rendered-card (render-card card)]
    (assoc
     rendered-card
     :pixi.container/children
     (if (and (:card/revealed rendered-card) (:player/selected? player))
       [(render-attack (:impi/key rendered-card) attack)]
       [])
     :pixi.sprite/tint
     (if (and (:card/revealed rendered-card)
              (:pixi.sprite/tint card)
              (:player/selected? player))
       (:pixi.sprite/tint card)
       0xFFFFFF))))

(defn render-player [card {:player/keys [selected?]}]
  (let [rendered-card (-> (render-card card)
                          (assoc :pixi.event/pointer-move [:player/move]
                                 :pixi.event/pointer-down [:player/down]
                                 :pixi.event/pointer-up [:player/up]
                                 :pixi.object/interactive? true))]
    (if selected?
      (let [rotation (* -2 js/PIXI.DEG_TO_RAD)]
        {:impi/key         :player-card
         :pixi.object/type :pixi.object.type/container
         :pixi.container/children
         [{:impi/key             :player/drop-shadow
           :pixi.object/type     :pixi.object.type/sprite
           :pixi.object/position (:pixi.object/position card)
           :pixi.object/rotation rotation
           :pixi.sprite/anchor   [0.5 0.5]
           :pixi.sprite/texture
           {:pixi.texture/source "img/dropshadow.png"}}
          (-> rendered-card
              (update :pixi.object/position #(mapv - % [4 4]))
              (assoc :pixi.object/rotation rotation))]})
      rendered-card)))

(defn render-field [{:keys [game-state player field]} field-x]
  (let [{:keys [deck position]}       (last game-state)
        [c r]                         position
        attacks                       (core/apply-attacks deck)
        {:moves/keys [valid invalid]} (core/moves game-state)]
    {:impi/key             :game/field
     :pixi.object/type     :pixi.object.type/container
     :pixi.object/position [field-x (:field/y field)]
     :pixi.container/children
     [{:impi/key         :deck
       :pixi.object/type :pixi.object.type/container
       :pixi.container/children
       (->> (for [[r-idx row] (map-indexed vector deck)]
              (for [[c-idx card] (map-indexed vector row)
                    :let         [index   [c-idx r-idx]
                                  attack  (core/lookup-card attacks index)
                                  flipped (get-in field
                                                  [:field/cards
                                                   index
                                                   :flipped])]]
                [index (render-field-card
                        (merge card
                               {:pixi.object/position (utils/card-pos index)
                                :pixi.object/index    index
                                ;;:revealed (<= r-idx (+ r 3))
                                :pixi.sprite/tint
                                (cond
                                  (contains? (set (vals valid)) index)   0xccffcc
                                  (contains? (set (vals invalid)) index) 0xffcccc)
                                :flipped              flipped
                                :attack               attack})
                        player)]))
            (into (sorted-map-by >)))}
      (render-player (assoc core/player-card
                            :pixi.object/position
                            (:player/pos player))
                     player)]}))

(defn render-hand [hand pos]
  {:impi/key             :game/hand
   :pixi.object/type     :pixi.object.type/container
   :pixi.object/position pos
   :pixi.container/children
   (map-indexed
    (fn [i c]
      {:impi/key             (str "game/hand-" i)
       :pixi.object/type     :pixi.object.type/container
       :pixi.object/rotation (let [from -15
                                   to   15
                                   n    (count hand)]
                               (* (+ from (* (* (/ (Math/abs (- from to)) (dec n))) i))
                                  js/PIXI.DEG_TO_RAD))
       :pixi.object/position [(* i utils/card-w 0.5) (* utils/card-h 0.5)]
       :pixi.container/children
       [{:impi/key             (str "hand/drop-shadow-" i)
         :pixi.object/type     :pixi.object.type/sprite
         :pixi.sprite/anchor   [0.5 0.86]
         :pixi.sprite/texture
         {:pixi.texture/source "img/dropshadow.png"}}
        (render-card (assoc c :pixi.sprite/anchor [0.5 0.9]))]})
    hand)})

(defn render-state
  [{:keys [canvas field game-state] :as state}]
  {:pixi/renderer
   (let [{:canvas/keys [color]} canvas]
     {:pixi.renderer/size             [js/window.innerWidth js/window.innerHeight]
      :pixi.renderer/background-color color
      :pixi.renderer/transparent?     false
      :pixi.renderer/antialias?       true})
   :impi/events-chan events-chan
   :pixi/stage
   (let [field-x (- (* js/window.innerWidth 0.35)
                    (+ utils/card-w utils/card-spacing))]
     {:impi/key         :stage
      :pixi.object/type :pixi.object.type/container
      :pixi.container/children
      [(render-hand (-> game-state last :hand)
                    [(+ field-x
                        (+ (* utils/card-w 0.3)
                           (* 3 (+ utils/card-w utils/card-spacing))))
                     (- (* (+ utils/card-h utils/card-spacing) 4)
                        (/ utils/card-h 2))])
       (render-field state field-x)]})})

(defn init-stage! []
  (reset!
   state
   (let [game-state (core/init-game-state)]
     {:game-state game-state

      :canvas
      #:canvas {:color 0x00cc66 #_0x0a1c5e}
      :player
      #:player {:selected? false
                :pos       (utils/card-pos (-> game-state last :position))}
      :field
      #:field  {:cards (->> (for [[r-idx row] (map-indexed vector (-> game-state last :deck))]
                              (for [[c-idx card] (map-indexed vector row)]
                                [[c-idx r-idx] {:flipped 180}]))
                            (into {}))
                :y     (- (* (+ utils/card-h utils/card-spacing) 3)
                          (/ utils/card-h 2))}})))

(defn take-all! [chan]
  (loop [elements []]
    (if-let [element (async/poll! chan)]
      (recur (conj elements element))
      elements)))

(defn game-handler [delta-time]

  ;; process all the new events
  ;; may generate updates/animations/events
  (doseq [e (take-all! events-chan)]
    (events/handler-event channels e))

  (let [updates           (take-all! updates-chan)
        animations        (take-all! animations-chan)
        new-animations    (mapv (partial animate/apply-animation delta-time)
                                animations)
        animation-updates (mapv :update-fn new-animations)
        all-updates       (seq (concat updates animation-updates))]

    ;; process any animations
    (doseq [a new-animations]
      (when (or (seq (:children a)) (seq (dissoc a :update-fn :children)))
        (async/put! animations-chan a)))

    #_(when (seq animations)
        (println delta-time)
        (println animations))

    ;; process all the updates
    (when all-updates
      (swap! state (apply comp all-updates)))))

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
  (init-stage!))
