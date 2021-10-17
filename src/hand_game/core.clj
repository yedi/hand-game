(ns hand-game.core)

(defn new-player []
  {:left 1
   :right 1})

(defn new-game []
  {:player-1 (new-player)
   :player-2 (new-player)
   :turn :player-1})

(def MAX_HAND_VALUE 5)

;; moves are as followed
;; [:hit {:attack :right :with :left}]
;; [:hit {:attack :left :with :left}]
;; [:split {:left 1 :right 2}]

(defn hit
  "Returns the result of hitting with attack-hit-args against defense-hit-args. If the
   result is greater than MAX_HAND_hit-args the hand is knocked out and considered nil"
  [attack-hit-args defense-hit-args]
  (when (and attack-hit-args defense-hit-args)
    (let [result (+ attack-hit-args defense-hit-args)]
      (when (< result MAX_HAND_VALUE)
        result))))

(defn hand-split-value [player]
  (+ (or (:left player) 0)
     (or (:right player) 0)))

(def valid-split? (set (range 1 MAX_HAND_VALUE)))

(defn opponent [player]
  ({:player-1 :player-2
    :player-2 :player-1} player))

(defn valid-move?
  "Given a game state and a move, returns whether the move is valid for the game"
  [game move]
  (let [[action hit-args] move
        player (:turn game)
        opponent (opponent player)]
    (cond (= action :hit)
          (let [{:keys [attack with]} hit-args]
            (and (some? (with (player game)))
                 (some? (attack (opponent game)))))

          (= action :split)
          (and
           (every? valid-split? (vals hit-args))
           (not= (player game) hit-args)
           (= (hand-split-value hit-args)
              (hand-split-value (player game)))))))

(defn handle-move [game move]
  (let [[action hit-args] move
        player (:turn game)
        opponent (opponent player)]
    (if-not (valid-move? game move)
      game
      (let [next-state (assoc game :turn opponent)]
        (case action
          :hit
          (let [{:keys [attack with]} hit-args]
            (update-in next-state
                       [opponent attack]
                       (partial hit (with (player game)))))

          :split
          (assoc next-state player hit-args))))))

(defn winner [game]
  (let [losing-hands {:left nil :right nil}]
    (cond (= losing-hands (:player-1 game))
          :player-2

          (= losing-hands (:player-2 game))
          :player-1)))

(defn handle-turn [game move]
  (if (winner game)
    game
    (handle-move game move)))

(defn heuristic [player game]
  (cond (= (winner game) player)
        1
        (some? (winner game))
        -1
        :else
        0))

(defn possible-splits [hands]
  (let [split-value (hand-split-value hands)
        left-values (range (max 1 (- split-value (dec MAX_HAND_VALUE)))
                           (min MAX_HAND_VALUE split-value))]
    (->> left-values
         (map (fn [left-value]
                {:left left-value :right (- split-value left-value)}))
         (remove (fn [possible-hands]
                   (= (set (vals possible-hands))
                      (set (vals hands)))))
         (map #(vector :split %)))))

(defn possible-hits [player-hands opponent-hands]
  (for [attack (->> opponent-hands
                    (map (fn [[hand value]] (when value hand)))
                    (remove nil?))
        with (->> player-hands
                  (map (fn [[hand value]] (when value hand)))
                  (remove nil?))]
    [:hit {:attack attack :with with}]))

(defn possible-moves [game]
  (let [player (:turn game)
        opponent (opponent player)]
    (concat (possible-splits (player game))
            (possible-hits (player game) (opponent game)))))

; function  minimax(node, depth, maximizingPlayer) is
;    if depth = 0 or node is a terminal node then
;        return the heuristic value of node
;    if maximizingPlayer then
;        value := −∞
;        for each child of node do
;            value := max(value, minimax(child, depth − 1, FALSE))
;        return value
;    else (* minimizing player *)
;        value := +∞
;        for each child of node do
;            value := min(value, minimax(child, depth − 1, TRUE))
;        return value

(defn minimax [game-node depth player]
  (let [max-player? (= player (:turn game-node))]
    ;(println max-player?)
    (cond (or (zero? depth) (winner game-node))
          (heuristic player game-node)

          max-player?
          (->> (possible-moves game-node)
               (map (partial handle-turn game-node))
               (map #(minimax % (dec depth) player))
               (apply max))

          :else
          (->> (possible-moves game-node)
               (map (partial handle-turn game-node))
               (map #(minimax % (dec depth) player))
               (apply min)))))

(defn move-score [game player move]
  (minimax (handle-turn game move) 5 player))

(defn best-move [game]
  (let [player (:turn game)
        move-scores (->> (possible-moves game)
                         (map #(vector % (move-score game player %)))
                         (into {}))
        max-score (apply max (vals move-scores))]
    (clojure.pprint/pprint move-scores)
    (->> move-scores
         (filter (fn [[_ score]] (= score max-score)))
         first
         first)))

(defn ai-move [game]
  (handle-turn game (best-move game)))

(defn possible-move-scores [game]
  (->> (possible-moves game)
       (map #(vector % (move-score game (:turn game) %)))
       (into {})))

(comment
 (def game (new-game))

 (def problem-game {:player-1 {:left nil, :right 3},
                    :player-2 {:left 2, :right 1},
                    :turn :player-1,
                    :winner nil})

 (def problem-game-2 {:player-1 {:left 3, :right 1},
                      :player-2 {:left nil, :right 1},
                      :turn :player-2})

 (possible-move-scores problem-game)

 (ai-move (new-game))

 (handle-turn game [:hit {:attack :right :with :right}])
 (-> game
     (handle-turn [:hit {:attack :right :with :right}])
     (handle-turn [:hit {:attack :right :with :right}])
     (handle-turn [:hit {:attack :right :with :right}])
     (handle-turn [:hit {:attack :right :with :right}])
     (handle-turn [:hit {:attack :right :with :right}])
     (handle-turn [:hit {:attack :right :with :right}]))

 ;; this is an infinite list of moves made by the AI
 (def perfect-game (iterate ai-move (new-game)))

 (take 1000 perfect-game)

 (def hand-values [nil 1 2 3 4])
 (def all-game-states
   (for [turn [:player-1 :player-2]
         player-1-left hand-values
         player-1-right hand-values
         player-2-left hand-values
         player-2-right hand-values]
     {:turn turn
      :player-1 {:left player-1-left :right player-1-right}
      :player-2 {:left player-2-left :right player-2-right}}))

 (def player-1-turn-scores
   (->> all-game-states
        (remove #(zero? (minimax :player-1 %)))
        (map #(vector % (minimax :player-1 %)))
        (into {})))

 (def player-1-game-scores
   (->> all-game-states
        (map #(vector % (minimax % 5 :player-1)))
        (into {}))))
