(ns forest-run.animate)

(defn ease-in [power t]
  (Math/pow t power))

(defn ease-out [power t]
  (- 1 (Math/abs (Math/pow (dec t) power))))

(defn ease-in-out [power t]
  (if (< t 0.5)
    (/ (ease-in power (* t 2)) 2)
    (+ (/ (ease-out power (dec (* t 2))) 2) 0.5)))

(defn apply-animation-steps
  [delta-time {[{:keys [progress duration update-gen]}] :steps
               :as node}]
  (let [t         (min (/ progress duration) 1)
        update-fn (update-gen t)]
    (if (< t 1)
      (-> node
          (update-in [:steps 0 :progress]
                     #(+ % delta-time))
          (assoc :update-fn update-fn))
      (if (> (count (:steps node)) 1)
        (apply-animation-steps
         (- progress duration)
         (-> node
             (update :steps #(into [] (rest %)))
             (assoc :update-fn update-fn)))
        (-> node
            (dissoc :steps)
            (assoc :update-fn update-fn))))))

(defn apply-animation
  [delta-time {:keys [steps children] :as node}]
  (if steps
    (apply-animation-steps delta-time node)
    (let [children      (mapv (partial apply-animation delta-time) children)
          new-children  (mapv #(dissoc % :update-fn) children)]
      (cond-> {:update-fn (apply comp (mapv :update-fn children))}
        (seq new-children) (assoc :children new-children)))))
