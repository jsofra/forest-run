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
  (let [t           (min (/ progress duration) 1)
        step-update (update-gen t)]
    (if (< t 1)
      (-> node
          (update-in [:steps 0 :progress]
                     #(+ % delta-time))
          (assoc :update step-update))
      (if (> (count (:steps node)) 1)
        (apply-animation-steps
         (- progress duration)
         (-> node
             (update :steps #(into [] (rest %)))
             (assoc :update step-update)))
        (-> node
            (dissoc :steps)
            (assoc :update step-update))))))

(defn comp-reactions [reactions]
  (fn [& args]
    (doseq [reaction reactions :when reaction]
      (apply reaction args))))

(defn apply-animation
  [delta-time {:keys [name steps children] :as node}]
  (if steps
    (apply-animation-steps delta-time node)
    (let [children     (mapv (partial apply-animation delta-time) children)
          new-children (mapv #(dissoc % :update) children)
          updates      (mapv :update children)
          reactions    (map :reaction updates)]
      (cond-> {:msg/type :animation
               :name     name
               :update   {:msg/type  :update
                          :update-fn (apply comp (mapv :update-fn updates))}}
        (seq new-children) (assoc :children new-children)
        (seq reactions)    (assoc-in [:update :reaction] (comp-reactions reactions))))))
