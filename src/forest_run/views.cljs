(ns forest-run.views
  (:require [forest-run.core :as core]
            [forest-run.ui-state-utils :as utils]))

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
     {:impi/key         card-name
      :card/flipped     flipped
      :card/revealed    revealed
      :pixi.object/type :pixi.object.type/container
      :pixi.container/children
      [{:impi/key           (str card-name "-shadow")
        :pixi.object/type   :pixi.object.type/sprite
        :pixi.object/alpha  0.3
        :pixi.sprite/anchor [0.5 0.5]
        :pixi.sprite/texture
        {:pixi.texture/source "img/dropshadow.png"}}
       (merge
        card-defaults
        {:impi/key         (str card-name "-card")
         :pixi.object/type :pixi.object.type/sprite
         :pixi.sprite/texture
         {:pixi.texture/source (if revealed
                                 (str "img/" (name card-name) ".png")
                                 (str "img/back.png"))}})]})))

(defn render-field-card
  [{:keys [attack] :as card} player]
  (let [rendered-card (render-card card)]
    (-> rendered-card
        (assoc-in [:pixi.container/children 1 :pixi.sprite/tint]
               (if (and (:card/revealed rendered-card)
                        (:pixi.sprite/tint card)
                        (:player/selected? player))
                 (:pixi.sprite/tint card)
                 0xFFFFFF))
        (cond-> (and (:card/revealed rendered-card)
                     (:player/selected? player))
          (update :pixi.container/children
                  conj
                  (render-attack (:impi/key rendered-card) attack))))))

(defn render-player [card {:player/keys [selected? pos pulse]}]
  (let [rendered-card (-> (render-card card)
                          (assoc :pixi.event/pointer-move [:player/move]
                                 :pixi.event/pointer-down [:player/down]
                                 :pixi.event/pointer-up [:player/up]
                                 :pixi.object/interactive? true
                                 :pixi.object/position pos))]
    (if selected?
      (let [rotation (* -2 js/PIXI.DEG_TO_RAD)]
        {:impi/key         :player/selected-card
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
      {:impi/key :player/card
       :pixi.object/type :pixi.object.type/container
       :pixi.container/children
       [{:impi/key             :player/pulse
         :pixi.object/type     :pixi.object.type/sprite
         :pixi.object/position pos
         :pixi.object/alpha    pulse
         :pixi.sprite/anchor   [0.5 0.5]
         :pixi.sprite/tint     0x00FF00
         :pixi.sprite/texture
         {:pixi.texture/source "img/card-glow.png"}}
        rendered-card]})))

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
                (when-not (or (:action card)
                              (= core/player-card card))
                  [index (assoc
                          (render-field-card
                           (merge card
                                  {;;:revealed (<= r-idx (+ r 3))
                                   :pixi.sprite/tint
                                   (cond
                                     (contains? (set (vals valid)) index)   0xccffcc
                                     (contains? (set (vals invalid)) index) 0xffcccc)
                                   :flipped flipped
                                   :attack  attack})
                           player)
                          :pixi.object/position (utils/card-pos index))])))
            (apply concat)
            (into (sorted-map-by >)))}
      (render-player core/player-card player)]}))

(defn sort-cards [cards]
  (sort-by (juxt :suit (comp core/face-values :rank)) cards))

(defn render-hand [hand pos]
  {:impi/key             :game/hand
   :pixi.object/type     :pixi.object.type/container
   :pixi.object/position pos
   :pixi.container/children
   (let [hand (map-indexed vector (partition-by :suit (sort-cards hand)))]
     (mapcat
      (fn [[suit-i cards]]
        (map-indexed
         (fn [i c]
           {:impi/key             (str "game/hand-" suit-i "-" i)
            :pixi.object/type     :pixi.object.type/container
            :pixi.object/rotation (let [n     (count cards)
                                        angle (* 15 (/ (dec n) 7))
                                        from  (* -1 angle)
                                        to    angle]
                                    (if (= n 1)
                                      0
                                      (* (+ from (* (* (/ (Math/abs (- from to)) (dec n))) i))
                                         js/PIXI.DEG_TO_RAD)))
            :pixi.object/position [(* i (/ 220 (count cards)))
                                   (* suit-i (* 0.9 utils/card-h))]
            :pixi.container/children
            [(-> (render-card c)
                 (assoc :pixi.object/pivot [0 (* utils/card-h 0.7)])
                 (update :pixi.container/children
                         conj
                         {:impi/key             (str "game/hand-" suit-i "-" i "-text")
                          :pixi.object/type     :pixi.object.type/text
                          :pixi.text/anchor     [3.5 3.1]
                          :pixi.text/text       (str (core/face-values (:rank c)))
                          :pixi.text/style
                          {:pixi.text.style/align            "left"
                           :pixi.text.style/fill             0x1a77ba
                           :pixi.text.style/font-weight      "normal"
                           :pixi.text.style/font-family      "Arial"
                           :pixi.text.style/font-size        28
                           ;:pixi.text.style/stroke           0x000000
                           ;:pixi.text.style/stroke-thickness 4
                           }}))]})
         cards))
      hand))})

(defn render-state
  [msg-chan {:keys [canvas game-state] :as state}]
  {:pixi/renderer
   (let [{:canvas/keys [color]} canvas]
     {:pixi.renderer/size             [js/window.innerWidth js/window.innerHeight]
      :pixi.renderer/background-color color
      :pixi.renderer/transparent?     false
      :pixi.renderer/antialias?       true})
   :impi/events-chan msg-chan
   :pixi/stage
   (let [field-x (* js/window.innerWidth 0.13)]
     {:impi/key          :stage
      :pixi.object/type  :pixi.object.type/container
      :pixi.object/scale (let [d 850]
                           [(/ js/window.innerWidth d)
                            (/ js/window.innerWidth d)])
      :pixi.container/children
      [(render-hand (-> game-state last :hand)
                    [(+ field-x
                        (+ utils/card-w
                           (* 2.5 (+ utils/card-w utils/card-spacing))))
                     (- (* (+ utils/card-h utils/card-spacing) 2)
                        (* 0.2 utils/card-h))])
       (render-field state field-x)]})})
