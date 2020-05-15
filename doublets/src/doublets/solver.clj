(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn dist [word1 word2] 
  (->> (map = word1 word2) (filter false?) (count)))
  
(defn candidates [word stack]
  (into []
        (comp (filter #(= (count word) (count %)))
              (filter #(= 1 (dist word %)))
              (filter #(= -1 (.indexOf stack %))))
        words))

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
