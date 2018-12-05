(ns forest-run.viz-events
  (:require [specviz.graphviz :as graphviz]
            [clojure.string :as string]
            [clojure.spec.alpha :as s]))

(s/def ::graphviz/shape #{"record" "box" "oval" "plaintext" "circle" "diamond"
                          "trapezium" "square" "folder" "doublecircle" "point"
                          "Mcircle" "cds" "tripleoctagon" "rarrow" "cylinder"
                          "star" "Msquare" "rnastab" "parallelogram"})

(defn clean-name
  "Turn the qualified keyword into a graphviz friendly name"
  [qkw]
  (when qkw
    (-> (apply str (namespace qkw) " " (name qkw))
        (string/replace ">" " ")
        (string/replace "." " ")
        (string/replace ":" " ")
        (string/replace "-" " ")
        (string/replace "?" " ")
        (string/split #" ")
        (->> (map string/capitalize)
             (apply str)))))

(defn events-map->elements [events-map]
  (concat (mapcat (fn [[event parents]]
                    (for [parent parents]
                      {::graphviz/from  (clean-name parent)
                       ::graphviz/label ""
                       ::graphviz/to    (clean-name (:key event))}))
                                 events-map)
          (map (fn [event]
                 (let [node-name (clean-name (:key event))]
                   (merge
                    {::graphviz/name  node-name
                     ::graphviz/label node-name}
                    (case (:type event)
                      :event     {::graphviz/shape     "cds"
                                  ::graphviz/style     "filled"
                                  ::graphviz/fillcolor "#F4D03F"}
                      :update    {::graphviz/shape     "cylinder"
                                  ::graphviz/style     "filled"
                                  ::graphviz/fillcolor "#85C1E9"}
                      :animation {::graphviz/shape     "tripleoctagon"
                                  ::graphviz/style     "filled"
                                  ::graphviz/fillcolor "#C39BD3"}))))
               (keys events-map))
          [{::graphviz/name      "GameStart"
            ::graphviz/label     "GameStart"
            ::graphviz/shape     "oval"
            ::graphviz/style     "filled"
            ::graphviz/fillcolor "#2ECC71"}
           {::graphviz/name      (clean-name :game/UI)
            ::graphviz/label     (clean-name :game/UI)
            ::graphviz/shape     "parallelogram"
            ::graphviz/style     "filled"
            ::graphviz/fillcolor "#D6DBDF"}]))

(defn graph-events! [events-map]
  (let [elements (events-map->elements events-map)
        svg (-> (graphviz/dot-string elements)
                (graphviz/generate-image! "events"))]
    (set! (.-innerHTML (.getElementById js/document "graphviz")) svg)))
