(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn seqpad [n coll]
  (concat coll (repeat (- n (count coll)) nil)))

(defn interleave-all [c1 c2]
  (let [len (reduce max (map count [c1 c2]))]
    (apply interleave (map (partial seqpad len) [c1 c2]))))

(def count-different
  (comp (partition-all 2)
        (map (partial apply =))
        (filter false?)
        (map (constantly 1))))

(defn dist [word1 word2] 
  (transduce count-different + (interleave-all word1 word2)))

(defn candidates [word stack]
  (->> words
       (filter #(= 1 (dist word %)))
       (filter #(= -1 (.indexOf stack %)))))

(defn doublets
  ([word1 word2] (doublets word1 word2 []))
  ([word1 word2 stack]
   (if (= word1 word2)
     (conj stack word1)
     (reduce
       (fn [_ w] 
         (let [res (doublets w word2 (conj stack word1))]
           (if (not-empty res) (reduced res) res)))
       []
       (candidates word1 stack)))))
