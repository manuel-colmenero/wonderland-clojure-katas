(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[:fox :goose :corn :you] [:boat] []])

(defn extract [& things]
  (fn [container]
    (into [] (remove (into #{:you} things)) container)))

(defn add [& things]
  (fn [container]
    (into container (cons :you things))))

(defn transform [stage fns]
  (into [] (map #(%1 %2) fns stage)))

(defn bring [thing]
  [[(extract thing) (add thing) identity]
   [identity (extract thing) (add thing)]])

(defn go-back [& things]
  [[identity (apply add things) (apply extract things)]
   [(apply add things) (apply extract things) identity]])

(defn river-crossing-plan []
  (reductions transform
              start-pos
              (concat
                (bring :goose)
                (go-back)
                (bring :fox)
                (go-back :goose)
                (bring :corn)
                (go-back)
                (bring :goose))))
