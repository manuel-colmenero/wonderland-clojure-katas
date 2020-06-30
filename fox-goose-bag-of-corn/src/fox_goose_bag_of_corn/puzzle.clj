(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :refer [subset? difference intersection union]]))

(def start-pos [[:fox :goose :corn :you] [:boat] []])

(defn extract [& things]
  (fn [container]
    (into [] (remove (into #{:you} things)) container)))

(defn add [& things]
  (fn [container]
    (into container (cons :you things))))

(defn transform [stage fns]
  (into [] (map #(%1 %2) fns stage)))

(defn safe? [things]
  (let [s (set things)]
    (or (contains? s :you)
        (every? #(not (subset? % s)) [#{:corn :goose} #{:fox :goose}]))))

(defn valid? [stage]
  (every? safe? stage))

(defn find-you [stage]
  (first (filter some? (map-indexed #(when ((set %2) :you) %1) stage))))

(defn neighbours [stage]
  (map (partial transform stage)
       (let [index (find-you stage)
             things (difference (set (stage index)) #{:you :boat})]
         (case index
           0 (map #(vector (extract %) (add %) identity) things)
           1 [[(apply add things) (apply extract things) identity]
              [identity (apply extract things) (apply add things)]]
           2 (concat [[identity (add) (extract)]]
                     (map #(vector identity (add %) (extract %)) things))))))

(defn score [stage]
  (let [things #{:you :fox :goose :corn}]
    (reduce +
            (map-indexed
              (fn [idx c] (-> idx stage set (intersection things) count (* c)))
              [2 1]))))

(defn abs [n]
  (if (neg? n) (- n) n))

(defn dist [s1 s2]
  (abs (apply - (map score [s1 s2]))))

(defn reconstruct-path [came-from end]
  (loop [current end
         path '()]
    (let [next-path (cons current path)]
      (if-some [nxt (came-from current)]
        (recur nxt next-path)
        next-path))))

(defn river-crossing-plan []
  (loop [open-set #{start-pos}
         came-from {}
         g-score {start-pos 0}
         f-score {start-pos (score start-pos)}]
    (when (not-empty open-set)
      (let [[current] (apply min-key val (filter #(-> % key open-set) g-score))]
        (if (zero? (score current)) (reconstruct-path came-from current)
          (let [nbs (filter valid? (neighbours current))
                t-scores (zipmap nbs (map #(+ (g-score current) (dist current %)) nbs))
                candidates (filter (fn [[neighbor t-score]] (< t-score (or (g-score neighbor) ##Inf))) t-scores)]
            (recur
              (-> open-set (disj current) (union (-> candidates keys set)))
              (merge came-from (zipmap (keys candidates) (repeat current)))
              (merge g-score candidates)
              (merge f-score (zipmap (keys candidates) (map (fn [[k v]] (+ v (score k))) candidates))))))))))
