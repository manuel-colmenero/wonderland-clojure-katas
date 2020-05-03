(ns alphabet-cipher.coder)

(def ^:private letters (vec "abcdefghijklmnopqrstuvwxyz"))

(defn- get-row [l]
  (sequence
    (comp (drop (.indexOf letters l)) (take (count letters)))
    (cycle letters)))

(defn- encode-letter [[k l]]
  (nth (get-row l) (.indexOf letters k)))

(defn- decode-letter [[k l]]
  (nth letters (.indexOf (get-row k) l)))

(defn- decipher-letter [[c l]]
  (nth letters (.indexOf (get-row l) c)))

(defn- transform [mapper keyword message]
  (transduce
    (comp (partition-all 2) (map mapper)) str
    (interleave keyword message)))

(defn- uncycle [string]
  (some (fn [len]
          (let [chunk (take len string)]
            (if (every? #(= % (take (count %) chunk)) (partition-all len string))
              (reduce str chunk))))
        (range 1 (count string))))

(defn encode [keyword message]
  (transform encode-letter (cycle keyword) message))

(defn decode [keyword message]
  (transform decode-letter (cycle keyword) message))

(defn decipher [cipher message]
  (uncycle (transform decipher-letter cipher message)))
