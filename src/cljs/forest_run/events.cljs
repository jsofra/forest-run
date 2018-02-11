(ns forest-run.events
  (:require [cljs.core.async :as async]
            [forest-run.ui-state-utils :as utils]
            [forest-run.animate :as animate]))

(defmulti handler-event (fn [_ e] (:key e)))

(defmethod handler-event :player/move
  [{:keys [updates]} {:keys [event]}]
  (async/put! updates
              #(cond-> %
                 (-> % :player :player/selected?)
                 (utils/update-player
                  :player/pos
                  (fn [pos]
                    (mapv +
                          pos
                          [event.data.originalEvent.movementX
                           event.data.originalEvent.movementY]))))))

(defmethod handler-event :player/up
  [{:keys [updates events]} _]
  (async/put! updates #(utils/assoc-player % :player/selected? false))
  (async/put! events {:key :player/return :args {:duration 30}}))

(defmethod handler-event :player/down
  [{:keys [updates]} _]
  (async/put! updates #(utils/assoc-player % :player/selected? true)))

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
