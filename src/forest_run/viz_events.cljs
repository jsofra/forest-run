(ns forest-run.viz-events
  (:require [specviz.graphviz :as viz]))

(defn graph-events! [events]
  (let [svg (-> (viz/dot-string [{::viz/name  "start"
                                  ::viz/shape "circle"}])
                (viz/generate-image! "events"))]
    (set! (.-innerHTML (.getElementById js/document "graphviz")) svg)))
