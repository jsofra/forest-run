(ns forest-run.ui
  (:require [impi.core :as impi]
            [forest-run.core :as core]))

(enable-console-print!)

(defmethod impi/update-prop! :pixi.event/mouse-move [object index _ listener]
  (impi/replace-listener object "mousemove" index listener))

(def card-size [122 200])
(def card-spacing 8)

(defmethod impi/update-prop! :pixi.object/skew [object _ _ [x y]]
  (.set (.-skew object) x y))

(defn render-card [{:keys [rank suit revealed] :as card} pos]
  (let [card-name (keyword (str (name rank) "-" (name suit)))]
    {:impi/key                 card-name
     :pixi.object/type         :pixi.object.type/sprite
     :pixi.object/position     pos
     :pixi.object/skew         [0 (* 45 js/PIXI.DEG_TO_RAD)]
     :pixi.sprite/anchor       [0.5 0.5]
     :pixi.object/interactive? true
     :pixi.event/click         [:card-click card-name]
     :pixi.sprite/texture
     {:pixi.texture/source (if true #_revealed
                               (str "img/imgc/" (name card-name) ".png")
                               (str "img/imgc/back.png"))}}))

(defn render-deck [deck]
  (vec (flatten (for [[r-idx row] (map-indexed vector deck)]
                  (for [[c-idx card] (map-indexed vector row)]
                    (render-card card (mapv #(* %1 (+ %2 card-spacing))
                                            [c-idx r-idx]
                                            card-size)))))))

(def canvas-size 800)

(defonce gui-state (atom nil))
(defonce game-state (atom (core/init-game-state)))

(defn update-stage-pos!
  [delta]
  (swap! gui-state update-in [:pixi/stage :pixi.object/position] #(mapv + % delta)))

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
     :card-click (fn [_ id] (prn :click id))}
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
       :pixi.container/children (render-deck (-> @game-state last :deck))}}}}))

(defn update-gui-state! [game-state]
  (swap! gui-state
         update-in
         [:pixi/stage :deck :pixi.container/children]
         #(render-deck (-> game-state last :deck))))

(let [element (.getElementById js/document "app")]
  (impi/mount :game @gui-state element)
  (add-watch game-state ::gui (fn [_ _ _ s] (update-gui-state! s)))
  (add-watch gui-state ::mount (fn [_ _ _ s] (impi/mount :game s element))))



(comment
  (init-stage!))
