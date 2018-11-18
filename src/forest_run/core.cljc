(ns forest-run.core
  (:require [clojure.set :as clojure.set]))

(def ranks  [:uno :due :tre :quattro :cinque :sei :sette :fante :cavallo :re])

(def face-values (zipmap ranks (map inc (range))))

(def suits #{:spade :coppe :denari :bastoni})

(def deck (into #{} (for [r ranks s suits] {:rank r :suit s})))

(def player-card {:rank :uno :suit :spade})

(def starting-hand (for [s [:spade :coppe :bastoni]] {:rank :tre :suit s}))

(def starting-deck (clojure.set/difference deck (conj starting-hand player-card)))

(defn shuffled-deck []
  (->> starting-deck
       (map #(assoc % :revealed false))
       shuffle
       (partition 3)
       (mapv vec)))

(defn lookup-card [deck pos]
  (get-in deck (if pos (reverse pos) [nil])))

(defn update-card [deck pos card]
  (if (lookup-card deck pos)
    (assoc-in deck (reverse pos) card)
    deck))

(defn dimensions [matrix]
  [(count (first matrix)) (count matrix)])

(def re-attack
  [[0 0 0 0 0]
   [0 0 2 0 0]
   [0 2 4 2 0]
   [0 0 2 0 0]
   [0 0 0 0 0]])

(def cavallo-attack
  [[0 0 0 0 0]
   [0 0 0 0 0]
   [0 0 3 0 0]
   [0 0 0 0 0]
   [0 0 0 0 0]])

(defn indices [matrix]
  (let [[w h] (dimensions matrix)]
    (->> (for [y (range h)
               x (range w)]
           [x y])
         (partition w)
         (mapv #(into [] %)))))

(defn attack-indices [attack]
  (let [offset (->> (dimensions attack)
                    (map #(int (/ % 2))))]
    (zipmap (->> (indices attack)
                 (apply concat)
                 (mapv (fn [i]
                         (mapv - i offset))))
            (apply concat attack))))

(def avatar-moves
  {:nw [1 1] :n [0  1] :ne [-1 1]
   :w  [1 0]           :e  [-1 0]})

(def attacks
  {:re      re-attack
   :cavallo cavallo-attack})

(defn i->xy [matrix i]
  (let [[w _] (dimensions matrix)]
    [(mod i w) (int (/ i w))]))

(defn segment-attacks [deck idx segment]
  (->> segment
       flatten
       (map-indexed (fn [i {rank :rank}] [i rank]))
       (filter #((set (keys attacks)) (second %)))
       (mapv (fn [[i rank]]
               [idx
                (update (i->xy deck i) 1 + (* 3 idx))
                (into [] (reverse (rank attacks)))]))))

(defn deck-attacks [deck]
  (->> (partition 3 deck)
       (map-indexed (partial segment-attacks deck))
       (apply concat)))

(defn zero-attack-deck [deck]
  (let [[w h] (dimensions deck)]
    (mapv vec (partition w (repeat (* w h) 0)))))

(defn apply-attack [deck [seg-idx pos attack]]
  (reduce (fn [d [idx atk]]
            (let [pos       (reverse (map + idx pos))
                  seg-limit (* (inc seg-idx) 3)]
              (if (and (>= (first pos) (- seg-limit 3))
                       (< (first pos) seg-limit)
                       (get-in d pos nil))
                (update-in d pos + atk)
                d)))
          deck (attack-indices attack)))

(defn apply-attacks [deck]
  (reduce apply-attack
          (zero-attack-deck deck)
          (deck-attacks deck)))

#_(defn turn-costs [segment moves]
    {:water   (count moves)
     :food    (int (/ (count moves) 2))
     :attacks (map #(get-in (combined-segment-attacks segment) (reverse %)) moves)})

(defn hand-with-strengths [hand]
  (->> hand
       (filter #(= (:suit %) :spade))
       (mapv #(assoc % :strength (face-values (or (-> % :trade :rank)
                                                  (-> % :rank)))))))

(defn valid-positions [history deck]
  (clojure.set/difference
   (set (apply concat (indices deck)))
   (set (map :position history))))

(defn all-possible-moves [history deck position]
  (let [valid-positions (valid-positions history deck)]
    (reduce-kv (fn [m k v]
                 (let [move (mapv + v position)]
                   (if (valid-positions move)
                     (assoc m k move)
                     m)))
               {} avatar-moves)))

(defn moves [history]
  (let [{:keys [deck turn position hand health] :as state} (last history)

        attacks     (apply-attacks deck)
        all-moves   (all-possible-moves history deck position)
        hand        (hand-with-strengths hand)
        valid-move? (fn [[k v]]
                      (let [atk (get-in attacks (reverse v))]
                        (or (<= atk (reduce + (map :strength hand)))
                            (> health atk))))]
    {:moves/valid   (into {} (filter valid-move? all-moves))
     :moves/invalid (into {} (remove valid-move? all-moves))}))

(defn make-move [history move {:keys [trade]}]
  (let [{:keys [deck position hand] :as state} (last history)
        target (lookup-card deck move)
        action (cond
                 (#{:cavallo :re} (:rank target)) :action/attack
                 (= (:rank target) :fante)        :action/trade
                 :else                            :action/collect)]
    (conj history
          (-> state
              (assoc :action   action
                     :position move
                     :deck     (-> deck
                                   (update-card move player-card)
                                   (update-card position {:action action})))
              (cond-> (= action :action/collect)
                (update :hand conj target))))))

(defn init-game-state []
  [{:deck     (shuffled-deck)
    :hand     starting-hand
    :position [1 -1]
    :health   10}])
