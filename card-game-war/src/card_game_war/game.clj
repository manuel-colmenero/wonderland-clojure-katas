(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn lose-round [cards]
  (subvec cards 1))

(defn win-round [cards opponent stack]
  (as-> cards $
      (subvec $ 1)
      (apply conj $ stack)
      (conj $ (first opponent) (first cards))))

(defn draw-round [cards]
  (vec (drop 4 cards)))

(defn draw-stack [p1 p2 stack]
  (vec (concat (take 4 p1) (take 4 p2) stack)))

(defn play-round [[_ r1] [_ r2]]
  (compare (.indexOf ranks r1) (.indexOf ranks r2)))

(defn play-game [player1-cards player2-cards]
  (loop [p1 player1-cards
         p2 player2-cards
         stack []]
    (if (some empty? [p1 p2])
      [p1 p2]
      (case (play-round (first p1) (first p2))
        -1 (recur (lose-round p1) (win-round p2 p1 stack) [])
        1 (recur (win-round p1 p2 stack) (lose-round p2) [])
        0 (recur (draw-round p1) (draw-round p2) (draw-stack p1 p2 stack))))))

(defn shuffle-deck []
  (sequence (comp (partition-all (/ (count cards) 2)) (map vec))
            (shuffle cards)))

(defn -main []
  (let [[p1 p2] (shuffle-deck)]
    (play-game p1 p2)))
