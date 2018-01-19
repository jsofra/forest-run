(ns forest-run.ui
  (:require [forest-run.impi]
            [impi.core :as impi]
            [forest-run.core :as core]))

(enable-console-print!)

(def card-w 122)
(def card-h 200)
(def card-size [card-w card-h])
(def card-spacing 8)

(defn render-card [{:keys [rank suit] :as card} pos index revealed]
  (let [card-name (keyword (str (name rank) "-" (name suit)))]
    {:impi/key                 card-name
     :pixi.object/type         :pixi.object.type/sprite
     :pixi.object/position     pos
     :card/rotation            0
     :game/index               index
     :pixi.sprite/anchor       [0.5 0.5]
     :pixi.sprite/tint         0xFFFFFF
     :pixi.object/interactive? true
     :pixi.event/click         [:card-click index]
     :pixi.sprite/texture
     {:pixi.texture/source (if revealed
                             (str "img/" (name card-name) ".png")
                             (str "img/back.png"))}}))

(defn render-deck [{:keys [deck position]}]
  (let [[c r] position
        card-pos (fn [c-idx r-idx]
                   (mapv #(* %1 (+ %2 card-spacing))
                         [(Math/abs (- c-idx 2)) (* r-idx -1)]
                         card-size))]
    (->> (for [[r-idx row] (map-indexed vector deck)]
           (for [[c-idx card] (map-indexed vector row)]
             (render-card card
                          (card-pos c-idx r-idx)
                          [c-idx r-idx]
                          (<= r-idx (inc r)))))
         flatten
         (map (fn [card] [(:game/index card) card]))
         (into {position (-> (render-card core/player-card
                                          (card-pos c r)
                                          position
                                          true)
                             (assoc :pixi.event/pointer-down [:player-down position])
                             (assoc :pixi.event/pointer-up   [:player-up position])
                             (assoc :pixi.event/pointer-up-outside [:player-up position])
                             (dissoc :pixi.event/click))}))))

(defonce gui-state (atom nil))
(defonce game-state (atom (core/init-game-state)))

(defn update-stage-pos!
  [delta]
  (swap! gui-state update-in [:pixi/stage :pixi.object/position] #(mapv + % delta)))

(defn update-card
  [state id prop update-fn]
  (update-in state
             [:pixi/stage
              :pixi.container/children
              :deck
              :pixi.container/children
              id
              prop]
             update-fn))

(defn update-card!
  [idx prop update-fn]
  (swap! gui-state update-card idx prop update-fn))

(defn update-card-tints
  [update-fn state idxs tint]
  (reduce (fn [accum-state idx]
            (update-card accum-state
                         idx
                         :pixi.sprite/tint
                         #(update-fn % tint)))
          state
          idxs))

(def add-card-tints (partial update-card-tints +))
(def sub-card-tints (partial update-card-tints -))

(def valid-move-tint (- 0x99ff99 0xFFFFFF))
(def invalid-move-tint (- 0xffb2b2 0xFFFFFF))

(defn update-move-tints
  [state tint-fn]
  (let [{:moves/keys [valid invalid]} (core/moves @game-state)
        valid-idxs                    (vals valid)
        invalid-idxs                  (vals invalid)]
    (-> state
        (tint-fn valid-idxs valid-move-tint)
        (tint-fn invalid-idxs invalid-move-tint))))

(def stage-x #(- (* js/window.innerWidth 0.5)
                 (+ card-w card-spacing)))

(defn init-stage! []
  (reset!
   gui-state
   {:pixi/renderer
    {:pixi.renderer/size             [js/window.innerWidth js/window.innerHeight]
     :pixi.renderer/background-color 0x0a1c5e
     :pixi.renderer/transparent?     false}
    :pixi/listeners
    {:mouse-move
     (fn [x]
       (let [event (-> x .-data .-originalEvent)]
         (when (and (not (zero? (.-buttons event)))
                    (not (zero? (.-movementY event))))
           (update-stage-pos! [0 (.-movementY event)]))))
     :card-click
     (fn [_ id] (update-card! id :card/rotation inc))
     :player-down
     (fn [x _]
       (swap! gui-state update-move-tints add-card-tints))
     :player-up
     (fn [x _]
       (swap! gui-state update-move-tints sub-card-tints))}
    :pixi/stage
    {:impi/key                 :stage
     :pixi.object/type         :pixi.object.type/container
     :pixi.object/position     [(stage-x) (- (* (+ card-h card-spacing) 3)
                                             (/ card-h 2))]
     :pixi.object/interactive? true
     :pixi.event/mouse-move    [:mouse-move]
     :pixi.container/children
     {:deck
      {:impi/key                :deck
       :pixi.object/type        :pixi.object.type/container
       :pixi.container/children (render-deck (last @game-state))}}}}))

(defn update-gui-state! [game-state]
  (swap! gui-state
         update-in
         [:pixi/stage :deck :pixi.container/children]
         #(render-deck (last game-state))))

(let [element (.getElementById js/document "app")]
  (impi/mount :game @gui-state element)
  (add-watch game-state ::gui (fn [_ _ _ s] (update-gui-state! s)))
  (add-watch gui-state ::mount (fn [_ _ _ s] (impi/mount :game s element))))

(defn resize-renderer! []
  (swap! gui-state
         #(-> %
              (assoc-in [:pixi/renderer :pixi.renderer/size]
                        [js/window.innerWidth js/window.innerHeight])
              (assoc-in [:pixi/stage :pixi.object/position 0]
                        (stage-x)))))

(.addEventListener js/window "resize" resize-renderer!)

(comment
  (init-stage!))
