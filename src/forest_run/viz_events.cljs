(ns forest-run.viz-events
  (:require [specviz.graphviz :as graphviz]))

(defn graph-events! [events]
  (let [svg (-> (graphviz/dot-string [{::graphviz/from "start"
                                       ::graphviz/label "foo"
                                       ::graphviz/to "end"}
                                      {::graphviz/name "start"
                                       ::graphviz/label ""
                                       ::graphviz/height 0.25
                                       ::graphviz/width 0.25
                                       ::graphviz/shape "circle"
                                       ::graphviz/style "filled"
                                       ::graphviz/fillcolor "#000000"}])
                (graphviz/generate-image! "events"))]
    (set! (.-innerHTML (.getElementById js/document "graphviz")) svg)))
