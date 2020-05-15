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

(defn next-word [word stack]
  (reduce
    (fn [_ w]
      (if (and (= 1 (dist w word)) (= -1 (.indexOf stack w)))
        (reduced w)))
    words))

(defn doublets [word1 word2]
  (loop [stack []
         w word1]
    (println w stack)
    (if (= w word2)
      (conj stack w)
      (if-let [found (next-word w stack)]
        (recur (conj stack w) found)
        []))))
