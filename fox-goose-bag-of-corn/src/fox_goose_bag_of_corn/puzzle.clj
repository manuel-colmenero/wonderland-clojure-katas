(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :refer [subset? difference]]))

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

(defn safe? [things]
  (let [s (set things)]
    (or (contains? s :you)
        (every? #(not (subset? % s)) [#{:corn :goose} #{:fox :goose}]))))

(defn valid? [stage]
  (every? safe? stage))

(defn find-you [stage]
  (first (filter some? (map-indexed #(when ((set %2) :you) %1) stage))))

; Where is :you? if...
; - it's in the first "island": bring anything on that island
; - if it's in the boat...
; -- alone? go back and forth
; -- with thing? bring back and forth
; - if it's in the island: bring something back or go back alone
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
