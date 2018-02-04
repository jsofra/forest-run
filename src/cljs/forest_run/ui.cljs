(ns forest-run.ui
  (:require [forest-run.impi]
            [impi.core :as impi]
            [forest-run.core :as core]
            [cljs.core.async :as async]))

(enable-console-print!)

(def card-w 122)
(def card-h 200)
(def card-size [card-w card-h])
(def card-spacing 8)

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

(defn render-card
  ([card]
   (render-card card nil))
  ([{:keys [rank suit pos index attack tint flipped]
     :or   {flipped 0}
     :as   card}
    player]
   (let [card-name (keyword (str (name rank) "-" (name suit)))
         flipped  (if (zero? (mod (+ flipped 90) 180)) (+ flipped 0.5) flipped)
         revealed (odd? (Math/ceil (/ (+ flipped 90) 180)))]
     {:impi/key                 card-name
      :pixi.object/type         :pixi.object.type/sprite
      :pixi.object/position     pos
      :card/flipped             flipped
      :card/revealed            revealed
      :game/index               index
      :pixi.sprite/anchor       [0.5 0.5]
      :pixi.sprite/tint         (if (and revealed tint (:player/selected? player))
                                  tint
                                  0xFFFFFF)
      :pixi.object/interactive? true
      :pixi.event/click         [:card-click index]
      :pixi.sprite/texture
      {:pixi.texture/source (if revealed
                              (str "img/" (name card-name) ".png")
                              (str "img/back.png"))}
      :pixi.container/children (if (and revealed (:player/selected? player))
                                 [(render-attack card-name attack)]
                                 [])})))

(defn render-player [{:keys [pos index] :as card}
                     {:player/keys [selected?]}]
  (let [rendered-card (-> (render-card card)
                          (assoc :pixi.event/pointer-move [:player/move]
                                 :pixi.event/pointer-down [:player/down]
                                 :pixi.event/pointer-up [:player/up]))]
    (if selected?
      (let [rotation (* -2 js/PIXI.DEG_TO_RAD)]
        {:impi/key :player-card
         :pixi.object/type :pixi.object.type/container
         :pixi.container/children
         [{:impi/key             :player/drop-shadow
           :pixi.object/type     :pixi.object.type/sprite
           :pixi.object/position pos
           :pixi.object/rotation rotation
           :pixi.sprite/anchor   [0.5 0.5]
           :pixi.sprite/texture
           {:pixi.texture/source "img/dropshadow.png"}}
          (-> rendered-card
              (update :pixi.object/position #(mapv - % [4 4]))
              (assoc :pixi.object/rotation rotation))]})
      rendered-card)))

(defn card-pos
  [c-idx r-idx]
  (mapv #(* %1 (+ %2 card-spacing))
        [(Math/abs (- c-idx 2)) (* r-idx -1)]
        card-size))

(defn render-cards [{:keys [game-state player turn]}]
  (let [{:keys [deck position]}       (last game-state)
        [c r]                         position
        attacks                       (core/apply-attacks deck)
        {:moves/keys [valid invalid]} (core/moves game-state)]
    [{:impi/key :deck
      :pixi.object/type :pixi.object.type/container
      :pixi.container/children
      (->> (for [[r-idx row] (map-indexed vector deck)]
             (for [[c-idx card] (map-indexed vector row)
                   :let         [index  [c-idx r-idx]
                                 attack (core/lookup-card attacks index)]]
               (render-card
                (merge card
                       {:pos      (apply card-pos index)
                        :index    index
                        ;;:revealed (<= r-idx (+ r 3))
                        :flipped  (get-in turn [:deck index :flipped] 180)
                        :attack   attack
                        :tint     (cond
                                    (contains? (set (vals valid)) index) 0xccffcc
                                    (contains? (set (vals invalid)) index) 0xffcccc)})
                player)))
           flatten
           (map (fn [card] [(:game/index card) card]))
           (into (sorted-map-by >)))}
     (render-player (merge core/player-card
                           {:pos   (:player/pos player)
                            :index position})
                    player)]))

(def stage-x #(- (* js/window.innerWidth 0.5)
                 (+ card-w card-spacing)))

(defonce state (atom nil))
(defonce updates-chan (async/chan))
(defonce animations-chan (async/chan))

(defn update-stage-y
  [state y-delta]
  (update-in state [:stage :stage/y] #(+ % y-delta)))

(defn assoc-player
  [state attr val]
  (assoc-in state [:player attr] val))

(defn update-player
  [state attr f]
  (update-in state [:player attr] f))

(defn render-state
  [{:keys [canvas stage game-state] :as state}]
  {:pixi/renderer
   (let [{:canvas/keys [color]} canvas]
     {:pixi.renderer/size             [js/window.innerWidth js/window.innerHeight]
      :pixi.renderer/background-color color
      :pixi.renderer/transparent?     false
      :pixi.renderer/antialias?       true})
   :pixi/listeners
   {:player/move
    (fn [e]
      (async/put! updates-chan #(cond-> %
                                  (-> % :player :player/selected?)
                                  (update-player
                                   :player/pos
                                   (fn [pos]
                                     (mapv +
                                           pos
                                           [e.data.originalEvent.movementX
                                            e.data.originalEvent.movementY]))))))
    :player/up
    (fn [_]
      (async/put! updates-chan #(assoc-player % :player/selected? false)))
    :player/down
    (fn [_]
      (async/put! updates-chan #(assoc-player % :player/selected? true)))}
   :pixi/stage
   {:impi/key                 :stage
    :pixi.object/type         :pixi.object.type/container
    :pixi.object/position     [(stage-x) (:stage/y stage)]
    :pixi.container/children
    {:cards
     {:impi/key                :cards
      :pixi.object/type        :pixi.object.type/container
      :pixi.container/children (render-cards state)}}}})

(defn init-stage! []
  (reset!
   state
   (let [game-state (core/init-game-state)]
     {:game-state game-state
      :canvas     #:canvas {:color  0x00cc66  #_0x0a1c5e}
      :stage      #:stage {:y (- (* (+ card-h card-spacing) 3)
                                 (/ card-h 2))}
      :player     #:player {:selected? false
                            :pos (apply card-pos (-> game-state last :position))}})))

(defn flip-animation [duration]
  {:children [{:steps [{:progress 0
                        :duration duration
                        :update-gen
                        (fn [t]
                          (fn [state]
                            (assoc-in state
                                      [:turn :deck [0 0] :flipped]
                                      (- 180 (* 180 t)))))}
                       {:progress 0
                        :duration duration
                        :update-gen
                        (fn [t]
                          (fn [state]
                            (assoc-in state
                                      [:turn :deck [0 0] :flipped]
                                      (* 180 t))))}]}]})


(let [element (.getElementById js/document "app")]
  (when @state (impi/mount :game (render-state @state) element))
  (add-watch state ::mount (fn [_ _ _ s]
                             (impi/mount :game (render-state s) element))))

(defn take-all! [chan]
  (loop [elements []]
    (if-let [element (async/poll! chan)]
      (recur (conj elements element))
      elements)))

(defn apply-animation-steps
  [delta-time [{:keys [progress duration update-gen]} :as steps]]
  (let [t         (min (/ progress duration) 1)
        update-fn (update-gen t)]
    (if (< t 1)
      {:steps     (update-in steps
                             [0 :progress]
                             #(+ % delta-time))
       :update-fn update-fn}
      (if (> (count steps) 1)
        {:steps     (into [] (rest steps))
         :update-fn update-fn}
        {:update-fn update-fn}))))

(defn apply-animation
  [delta-time {:keys [steps children]}]
  (if steps
    (apply-animation-steps delta-time steps)
    (let [children      (mapv (partial apply-animation delta-time) children)
          new-children  (mapv #(dissoc % :update-fn) children)]
      (cond-> {:update-fn (apply comp (mapv :update-fn children))}
        (seq new-children) (assoc :children new-children)))))

(defn game-handler [delta-time]
  (let [updates           (take-all! updates-chan)
        animations        (take-all! animations-chan)
        new-animations    (mapv (partial apply-animation delta-time)
                                animations)
        animation-updates (mapv :update-fn new-animations)
        all-updates       (seq (concat updates animation-updates))]

    (doseq [a new-animations]
      (when (or (seq (:children a)) (seq (dissoc a :update-fn :children)))
        (async/put! animations-chan a)))

    #_(when (seq animations)
        (println delta-time)
        (println animations))
    (when all-updates
      (swap! state (apply comp all-updates)))))

(defn add-ticker! []
  (doto (js/PIXI.ticker.Ticker.)
    (.stop)
    (.add game-handler)))

(defonce ticker (atom nil))

(defn start []
  (do (init-stage!)
      (reset! ticker (doto (add-ticker!)
                       (.start)))))

(defn pause []
  (if-let [ticker @ticker]
    (.stop ticker)))

(defn continue []
  (if-let [ticker @ticker]
    (.start ticker)))

(defn stop []
  (if-let [ticker @ticker]
    (doto ticker
      (.stop)
      (.destroy))))

(comment
  (init-stage!))
