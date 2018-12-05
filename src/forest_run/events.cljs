(ns forest-run.events
  (:require [cljs.core.async :as async]
            [forest-run.ui-state-utils :as utils]
            [forest-run.animate :as animate]
            [forest-run.core :as core]))

(defmulti handler-event (fn [e] (:key e)))

(defmethod handler-event :player/move
  [{:keys [event]}]
  [{:type      :update
    :key       :player/move-update
    :update-fn #(cond-> %
                  (-> % :player :player/selected?)
                  (utils/update-player
                   :player/pos
                   (fn [pos]
                     (mapv +
                           pos
                           [event.data.originalEvent.movementX
                            event.data.originalEvent.movementY]))))}])

(defmethod handler-event :player/set-pos
  [{{:keys [pos index]} :args}]
  [{:type      :update
    :key       :game/make-move-update
    :update-fn #(-> %
                    (utils/assoc-player :player/pos pos)
                    (update :game-state
                            core/make-move
                            index
                            {}))}])

(defmethod handler-event :player/up
  [{:keys [key]}]
  [{:type      :update
    :key       :player/deselected-update
    :update-fn #(utils/assoc-player % :player/selected? false)
    :reaction
    (fn [old-state new-state]
      (let [{:moves/keys [valid]} (core/moves (-> new-state :game-state))
            valid-positions       (map utils/card-pos (vals valid))
            max-diffs             (->> valid-positions
                                       (map
                                        #(map (fn [x y]
                                                (Math/abs (- x y)))
                                              % (-> new-state :player :player/pos)))
                                       (map (partial apply max))
                                       (map vector valid-positions (vals valid)))
            [[pos index _]]       (filter #(>= 30 (last %)) max-diffs)]
        (if pos
          [{:type   :event
            :key    :player/set-pos
            :args   {:pos   pos
                     :index index}}]
          [{:type   :event
            :key    :player/return
            :args   {:duration 30}}])))}])

(defmethod handler-event :player/down
  [{:keys [key]}]
  [{:type   :update
    :key    :player/selected-update
    :update-fn
    #(if (get-in % [:game :game/started?])
       (utils/assoc-player % :player/selected? true)
       (assoc-in % [:game :game/started?] true))
    :reaction
    (fn [old-state new-state]
      (when (and (not (get-in old-state [:game :game/started?]))
                 (get-in new-state [:game :game/started?]))
        [{:type   :event
          :key    :cards/flip
          :args   {:duration 30}}]))}])

(defn flip-cards [duration]
  {:type :animation
   :key  :cards/flip-animation
   :children
   (->>
    (for [r-idx (range 3)]
      (for [c-idx (range 3)]
        (let [delay-factor (* duration 0.3)
              delay        (+ (* c-idx delay-factor) (* r-idx 3 delay-factor))]
          {:steps
           [{:progress   0
             :duration   delay
             :update-gen (fn [t]
                           {:type      :update
                            :key       :card/flip-wait-update
                            :update-fn identity})}
            {:progress 0
             :duration duration
             :update-gen
             (fn [t]
               {:type   :update
                :key    :card/flip-update
                :update-fn
                (fn [{{:game/keys [segment-index]} :game
                      :as                          state}]
                  (assoc-in state
                            [:field
                             :field/cards
                             [c-idx (+ r-idx (* segment-index 3))]
                             :flipped]
                            (- 180 (* 180 t))))})}]})))
    (apply concat)
    (into []))})

(defmethod handler-event :cards/flip
  [{{:keys [duration]} :args}]
  [(flip-cards duration)])


(defn return-player [duration]
  {:type  :animation
   :key   :player/return-animation
   :steps [{:progress 0
            :duration duration
            :update-gen
            (fn [t]
              (let [t (animate/ease-in-out 3 t)]
                {:type   :update
                 :key    :player/return-update
                 :update-fn
                 (fn step [{game-state                    :game-state
                            {:player/keys [init-pos pos]} :player
                            :as                           state}]
                   (if init-pos
                     (let [delta (mapv #(* (- %2 %1) t)
                                       init-pos
                                       (utils/card-pos
                                        (-> game-state last :position)))]
                       (-> state
                           (assoc-in [:player :player/pos] (mapv + init-pos delta))
                           (cond-> (= t 1) (update :player dissoc :player/init-pos))))
                     (step (assoc-in state [:player :player/init-pos] pos))))}))}]})

(defmethod handler-event :player/return
  [{{:keys [duration]} :args}]
  [(return-player duration)])


(defn pulse-player [duration]
  {:type       :animation
   :key        :player/pulse-animation
   :steps      [{:progress 0
                 :duration (* duration 0.5)
                 :update-gen
                 (fn [t]
                   {:type   :update
                    :key    :player/pulse-grow-update
                    :update-fn
                    (fn [state]
                      (assoc-in state [:player :player/pulse] t))})}
                {:progress 0
                 :duration (* duration 0.5)
                 :update-gen
                 (fn [t]
                   {:type   :update
                    :key    :player/pulse-shrink-update
                    :update-fn
                    (fn [state]
                      (assoc-in state [:player :player/pulse] (Math/abs (dec t))))})}]
   :post-steps (fn [{{:game/keys [started?]} :game}]
                 (when (not started?)
                   [{:type   :event
                     :key    :player/pulse
                     :args   {:duration duration}}]))})

(defmethod handler-event :player/pulse
  [{{:keys [duration]} :args}]
  [(pulse-player duration)])


(defn slide-field [duration]
  {:type       :animation
   :key        :field/slide-animation
   :steps      [{:progress 0
                 :duration duration
                 :update-gen
                 (fn [t]
                   (let [t (animate/ease-in-out 3 t)]
                     {:type   :update
                      :key    :field/slide-animation
                      :update-fn
                      (fn step [{{:field/keys [init-y y]} :field
                                 :as                      state}]
                        (if init-y
                          (let [delta (* (+ utils/card-h utils/card-spacing) 3 t)]
                            (-> state
                                (assoc-in [:field :field/y] (+ init-y delta))
                                (cond-> (= t 1) (update :field dissoc :field/init-y))))
                          (step (assoc-in state [:field :field/init-y]
                                          (get-in state [:field :field/y])))))}))}]
   :post-steps (fn [_]
                 [{:type   :event
                   :key    :cards/flip
                   :args   {:duration 30}}])})

(defmethod handler-event :field/slide
  [{{:keys [duration]} :args}]
  [(slide-field duration)])

(defmethod handler-event :game/next-segment
  [{{:keys [duration]} :args}]
  [{:type      :update
    :key       :game/round-update
    :update-fn #(update-in % [:game :game/segment-index] inc)}
   {:type   :event
    :key    :field/slide
    :args   {:duration 60}}])
