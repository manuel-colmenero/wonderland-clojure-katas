(ns magic-square.puzzle)

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn is-magic? [v]
  (let [rows (partition 3 v)
        cols (apply map list rows)
        diagonals (map #(map v %) [[0 4 8] [2 4 6]])
        sums (map #(reduce + %) (concat rows cols diagonals))]
    (apply = sums)))

(defn -magic-square
  ([values] (-magic-square [] values))
  ([head tail]
   (if (empty? tail)
     (when (is-magic? head)
       (into [] (comp (partition-all 3) (map vec)) head))
     (first
       (sequence
         (comp
           (map #(-magic-square (conj head %) (remove (partial = %) tail)))
           (filter some?))    
         tail)))))

(def magic-square (memoize -magic-square))
