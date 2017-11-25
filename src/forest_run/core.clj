(ns forest-run.core)

(def ranks  [:uno :due :tre :quattro :cinque :sei :sette :fante :cavallo :re])

(def face-values (zipmap ranks (map inc (range))))

(def suits #{:spade :coppe :denari :bastoni})

(def deck (into #{} (for [r ranks s suits] {:rank r :suit s})))

(def player-card {:rank :uno :suit :spade})

(def starting-hand (for [s [:spade :coppe :bastoni]] {:rank :tre :suit s}))

(def starting-deck (clojure.set/difference deck (conj starting-hand player-card)))

(defn shuffled-deck
  ([] (shuffled-deck 3))
  ([look-ahead]
   (->> starting-deck
        shuffle
        (partition 3)
        (mapv vec)
        (partition look-ahead)
        (mapv vec))))

(defn lookup-card [deck turn pos]
  (get-in deck (into [turn] (if pos (reverse pos) [nil]))))

(defn update-card [deck turn pos card]
  (if (lookup-card deck turn pos)
    (assoc-in deck (into [turn] (reverse pos)) card)
    deck))

(def zero-segment
  [[0 0 0]
   [0 0 0]
   [0 0 0]])

(def re-attack
  [[0 0 0 0 0]
   [0 0 2 0 0]
   [0 2 4 2 0]
   [0 0 2 0 0]
   [0 0 0 0 0]])

(def cavallo-attack
  [[0 0 0 0 0]
   [0 0 0 0 0]
   [1 2 3 2 1]
   [0 2 2 2 0]
   [1 0 1 0 1]])

(def avatar-moves
  {:nw [-1 -1] :n [0  -1] :ne [1 -1]
   :w  [-1  0]            :e  [1  0]})

(def attacks
  {:re      re-attack
   :cavallo cavallo-attack})

(defn accum-score-segment
  [segment [[x-offset y-offset] accum-segment]]
  (let [x-offset (- (count (first segment)) (inc x-offset))
        y-offset (- (count segment) (inc y-offset))]
    (mapv #(mapv + %1 (drop x-offset %2))
          segment (drop y-offset accum-segment))))

(defn i->xy [segment i]
  [(mod i (count segment)) (int (/ i (count (first segment))))])

(defn segment-attacks [segment]
  (->> segment
       flatten
       (map-indexed (fn [i {r :rank}] [i r]))
       (filter #((set (keys attacks)) (second %)))
       (map (fn [[i r]] [(i->xy segment i) (r attacks)]))))

(defn combined-segment-attacks [segment]
  (reduce accum-score-segment zero-segment (segment-attacks segment)))

(defn turn-costs [segment moves]
  {:water   (count moves)
   :food    (int (/ (count moves) 2))
   :attacks (map #(get-in (combined-segment-attacks segment) (reverse %)) moves)})

(defn valid-positions [segment]
  (set (for [y (range (count segment))
             x (range (count (first segment)))]
         [x y])))

(defn hand-with-strengths [hand]
  (->> hand
       (filter #(= (:suit %) :spade))
       (mapv #(assoc % :strength (face-values (or (-> % :trade :rank)
                                                  (-> % :rank)))))))

(defn all-possible-moves [valid-positions position]
  (reduce-kv (fn [m k v]
               (let [move (mapv + v position)]
                 (if (valid-positions move)
                   (assoc m k move)
                   m)))
             {} avatar-moves))

(defn moves [{:keys [deck turn position hand] :as state}]
  (let [segment         (nth deck turn)
        valid-positions (valid-positions segment)
        attacks         (combined-segment-attacks segment)
        all-moves       (all-possible-moves valid-positions position)
        valid-move?     (fn [[k v]]
                          (<= (get-in attacks (reverse v))
                              (reduce + (map :strength hand))))]
    {:moves/valid   (into {} (filter valid-move? all-moves))
     :moves/invalid (into {} (remove valid-move? all-moves))}))

(defn make-move [{:keys [deck turn position hand] :as state}
                 move
                 {:keys [trade]}]
  (let [target (lookup-card deck turn move)
        action (cond
                 (#{:cavallo :re} (:rank target)) :action/attack
                 (= (:rank target) :fante)        :action/trade
                 :else                            :action/collect)]
    (-> state
        (assoc :action   action
               :position move
               :deck     (-> deck
                             (update-card turn move player-card)
                             (update-card turn position {:action action})))
        (cond-> (= action :action/collect)
          (update :hand conj target)))))
