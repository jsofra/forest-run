(ns forest-run.ui
  (:require [impi.core :as impi]
            [forest-run.core :as core]))

(enable-console-print!)

(defmethod impi/update-prop! :pixi.event/mouse-move [object index _ listener]
  (impi/replace-listener object "mousemove" index listener))

(def card-size [122 200])
(def card-spacing 8)

(defmethod impi/update-prop! :card/rotation [object _ _ rotation]
  (.set (.-skew object) 0 (* rotation js/PIXI.DEG_TO_RAD)))

(defmethod impi/update-prop! :pixi.sprite/tint [object _ _ tint]
  (set! (.-tint object) tint))

(defn render-card [{:keys [rank suit] :as card} pos revealed]
  (let [card-name (keyword (str (name rank) "-" (name suit)))]
    {:impi/key                 card-name
     :pixi.object/type         :pixi.object.type/sprite
     :pixi.object/position     pos
     :card/rotation            0
     :pixi.sprite/anchor       [0.5 0.5]
     :pixi.sprite/tint         0xFFFFFF
     :pixi.object/interactive? true
     :pixi.event/click         [:card-click card-name]
     :pixi.sprite/texture
     {:pixi.texture/source (if revealed
                               (str "img/" (name card-name) ".png")
                               (str "img/back.png"))}}))

(defn render-deck [{:keys [deck position]}]
  (let [[c r] position]
    (->> (for [[r-idx row] (map-indexed vector (take 2 (drop r (reverse deck))))]
           (for [[c-idx card] (map-indexed vector (reverse row))]
             (do (println c-idx r-idx)
                 (render-card card
                              (mapv #(* %1 (+ %2 card-spacing))
                                    [c-idx r-idx]
                                    card-size)
                              (not= r-idx 0)))))
         flatten
         (map (fn [card] [(:impi/key card) card]))
         (into {}))))

(def canvas-size 800)

(defonce gui-state (atom nil))
(defonce game-state (atom (core/init-game-state)))

(defn update-stage-pos!
  [delta]
  (swap! gui-state update-in [:pixi/stage :pixi.object/position] #(mapv + % delta)))

(defn update-card!
  [id]
  (swap! gui-state
         update-in
         [:pixi/stage
          :pixi.container/children
          :deck
          :pixi.container/children
          id
          :card/rotation]
         inc))

(defn init-stage! []
  (reset!
   gui-state
   {:pixi/renderer
    {:pixi.renderer/size             [canvas-size canvas-size]
     :pixi.renderer/background-color 0x0a1c5e
     :pixi.renderer/transparent?     false}
    :pixi/listeners
    {:mouse-move (fn [x]
                   (let [event (-> x .-data .-originalEvent)]
                     (when (and (not (zero? (.-buttons event)))
                                (not (zero? (.-movementY event))))
                       (update-stage-pos! [0 (.-movementY event)]))))
     :card-click (fn [_ id] (update-card! id))}
    :pixi/stage
    {:impi/key                 :stage
     :pixi.object/type         :pixi.object.type/container
     :pixi.object/position     [100 100]
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



(comment
  (init-stage!))
