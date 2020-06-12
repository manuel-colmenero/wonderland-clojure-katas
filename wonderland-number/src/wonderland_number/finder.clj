(ns wonderland-number.finder)

(defn is-wonderland-number? [n]
  (apply = (map #(-> % (* n) str set) (range 2 7))))

(defn wonderland-number []
  (->> (range 100000 1000000) (filter is-wonderland-number?) first))
