(ns tiny-maze.solver)

(defn find-pos [maze kw]
  (some #(when (= kw (get-in maze %)) %)
        (apply concat
               (for [i (-> maze count range)]
                 (for [j (-> i maze count range)]
                   [i j])))))

(defn neighbours [[x y]]
  (map (fn [[i j]] [(+ x i) (+ y j)])
       [[-1 0] [1 0] [0 -1] [0 1]]))

(defn is-valid [maze coords]
  (#{0 :E} (get-in maze coords)))

(defn walk-recursive [maze coords]
  (let [next-maze (assoc-in maze coords :x)]
    (if (= (get-in maze coords) :E)
      next-maze
      (some #(walk-recursive next-maze %)
            (filter #(is-valid maze %) (neighbours coords))))))

(defn solve-maze [maze]
  (let [start-pos (find-pos maze :S)]
    (walk-recursive maze start-pos)))
