(ns forest-run.events
  (:require [cljs.core.async :as async]
            [forest-run.ui-state-utils :as utils]
            [forest-run.animate :as animate]))

(defmulti handler-event (fn [_ e] (:key e)))

(defmethod handler-event :player/move
  [{:keys [updates]} {:keys [event]}]
  (async/put! updates
              {:update-fn #(cond-> %
                             (-> % :player :player/selected?)
                             (utils/update-player
                              :player/pos
                              (fn [pos]
                                (mapv +
                                      pos
                                      [event.data.originalEvent.movementX
                                       event.data.originalEvent.movementY]))))}))

(defmethod handler-event :player/up
  [{:keys [updates events]} _]
  (async/put! updates {:update-fn #(utils/assoc-player % :player/selected? false)})
  (async/put! events {:key :player/return :args {:duration 30}}))

(defmethod handler-event :player/down
  [{:keys [updates events]} _]
  (async/put! updates
              {:update-fn
               #(if (get-in % [:game :game/started?])
                    (utils/assoc-player % :player/selected? true)
                    (assoc-in % [:game :game/started?] true))
               :reaction
               (fn [old-state new-state]
                 (when (and (not (get-in old-state [:game :game/started?]))
                            (get-in new-state [:game :game/started?]))
                   (async/put! events {:key :cards/flip :args {:duration 30}})))}))

(defn flip-cards [duration]
  {:children
   (->>
    (for [r-idx (range 3)]
      (for [c-idx (range 3)]
        (let [delay-factor (* duration 0.3)
              delay (+ (* c-idx delay-factor) (* r-idx 3 delay-factor))]
          {:steps
           [{:progress   0
             :duration   delay
             :update-gen (fn [t] identity)}
            {:progress 0
             :duration duration
             :update-gen
             (fn [t]
               (fn [state]
                 (assoc-in state
                           [:field :field/cards [c-idx r-idx] :flipped]
                           (- 180 (* 180 t)))))}]})))
    (apply concat)
    (into []))})

(defmethod handler-event :cards/flip
  [{:keys [animations]} {{:keys [duration]} :args}]
  (async/put! animations (flip-cards duration)))


(defn return-player [duration]
  {:steps [{:progress 0
            :duration duration
            :update-gen
            (fn [t]
              (let [t (animate/ease-in-out 3 t)]
                (fn step [{game-state :game-state
                           {:player/keys [init-pos pos]} :player
                           :as state}]
                  (if init-pos
                    (let [delta (mapv #(* (- %2 %1) t)
                                      init-pos
                                      (utils/card-pos
                                       (-> game-state last :position)))]
                      (-> state
                          (assoc-in [:player :player/pos] (mapv + init-pos delta))
                          (cond-> (= t 1) (update :player dissoc :player/init-pos))))
                    (step (assoc-in state [:player :player/init-pos] pos))))))}]})

(defmethod handler-event :player/return
  [{:keys [animations]} {{:keys [duration]} :args}]
  (async/put! animations (return-player duration)))


(defn pulse-player [duration]
  {:steps        [{:progress 0
                   :duration (* duration 0.5)
                   :update-gen
                   (fn [t]
                     (fn [state]
                       (assoc-in state [:player :player/pulse] t)))}
                  {:progress 0
                   :duration (* duration 0.5)
                   :update-gen
                   (fn [t]
                     (fn [state]
                       (assoc-in state [:player :player/pulse] (Math/abs (dec t)))))}]
   :post-steps (fn [{:keys [events]}
                    {{:game/keys [started?]} :game}]
                 (when (not started?)
                   (async/put! events {:key :player/pulse :args {:duration duration}})))})

(defmethod handler-event :player/pulse
  [{:keys [animations]} {{:keys [duration]} :args}]
  (async/put! animations (pulse-player duration)))


(defn slide-field [duration]
  {:steps [{:progress 0
            :duration duration
            :update-gen
            (fn [t]
              (let [t (animate/ease-in-out 3 t)]
                (fn step [{{:field/keys [init-y y]} :field
                           :as state}]
                  (if init-y
                    (let [delta (* (+ utils/card-h utils/card-spacing) 3 t)]
                      (-> state
                          (assoc-in [:field :field/y] (+ init-y delta))
                          (cond-> (= t 1) (update :field dissoc :field/init-y))))
                    (step (assoc-in state [:field :field/init-y]
                                    (get-in state [:field :field/y])))))))}]})

(defmethod handler-event :field/slide
  [{:keys [animations]} {{:keys [duration]} :args}]
  (async/put! animations (slide-field duration)))
