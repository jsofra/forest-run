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
               :as                                      node}]
  (let [t           (min (/ progress duration) 1)
        step-update (update-gen t)
        new-msgs    (if (< t 1)
                      [(update-in node [:steps 0 :progress] #(+ % delta-time))
                       step-update]
                      (if (> (count (:steps node)) 1)
                        (into (apply-animation-steps
                               (- progress duration)
                               (update node :steps #(into [] (rest %))))
                              [step-update])
                        [(dissoc node :steps)
                         step-update]))]
    (mapv #(assoc % :parent node) new-msgs)))

(defn comp-reactions [reactions]
  (fn [& args]
    (doseq [reaction reactions :when reaction]
      (apply reaction args))))

(defn apply-animation
  [delta-time {:keys [key steps children] :as node}]
  (if steps
    (apply-animation-steps delta-time node)
    (if (seq children)
      (let [child-msgs           (->> children
                                      (map (partial apply-animation delta-time))
                                      (apply concat))
            {updates      true
             new-children false} (group-by #(= (:type %) :update) child-msgs)
            reactions            (map :reaction updates)]
        (into (if (seq new-children)
                [{:type     :animation
                  :key      key
                  :parent   node
                  :children new-children}]
                [])
              updates))
      [])))
