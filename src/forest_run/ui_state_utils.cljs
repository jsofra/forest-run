(ns forest-run.ui-state-utils)

(def card-w 122)
(def card-h 200)
(def card-size [card-w card-h])
(def card-spacing 8)

(defn card-pos
  [[c-idx r-idx]]
  (mapv #(* %1 (+ %2 card-spacing))
        [(Math/abs (- c-idx 2)) (* r-idx -1)]
        card-size))

(defn assoc-player
  [state attr val]
  (assoc-in state [:player attr] val))

(defn update-player
  [state attr f]
  (update-in state [:player attr] f))
