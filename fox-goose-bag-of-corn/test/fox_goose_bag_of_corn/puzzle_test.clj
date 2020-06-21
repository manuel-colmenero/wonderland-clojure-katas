(ns fox-goose-bag-of-corn.puzzle-test
  (:require [clojure.test :refer :all]
            [fox-goose-bag-of-corn.puzzle :refer :all]
            [clojure.set]))

(deftest safe-test
  (testing "an island with you in it is always safe"
    (is (true? (safe? [:you :fox :goose])))
    (is (true? (safe? [:you :corn :goose])))
    (is (true? (safe? [:you :corn :fox]))))
  (testing "fox and corn are fine together"
    (is (true? (safe? [:fox :corn]))))
  (testing "an island is not safe"
    (is (false? (safe? [:fox :goose])))
    (is (false? (safe? [:corn :goose])))))

(deftest find-you-test
  (testing "it finds where you are"
    (is (= 0 (find-you [[:fox :goose :corn :you] [:boat] []])))
    (is (= 1 (find-you [[:fox :goose :corn] [:boat :you] []])))
    (is (= 2 (find-you [[:fox :goose :corn] [:boat] [:you]])))))

(deftest neighbours-test
  (testing "it finds the possible next steps"
    (is (= (neighbours [[:fox :goose :corn :you] [:boat] []])
           [[[:goose :corn] [:boat :you :fox] []]
            [[:fox :corn] [:boat :you :goose] []]
            [[:fox :goose] [:boat :you :corn] []]]))
    (is (= (neighbours [[:fox :goose :corn] [:boat :you] []])
           [[[:fox :goose :corn :you] [:boat] []]
            [[:fox :goose :corn] [:boat] [:you]]]))
    (is (= (neighbours [[:fox :goose] [:boat :you :corn] []])
           [[[:fox :goose :you :corn] [:boat] []]
            [[:fox :goose] [:boat] [:you :corn]]]))
    (is (= (neighbours [[:fox] [:boat] [:you :corn :goose]])
           [[[:fox] [:boat :you] [:corn :goose]]
            [[:fox] [:boat :you :goose] [:corn]]
            [[:fox] [:boat :you :corn] [:goose]]]))))

(defn validate-move [step1 step2]
  (testing "only you and another thing can move"
    (let [diff1 (clojure.set/difference step1 step2)
          diff2 (clojure.set/difference step2 step1)
          diffs (concat diff1 diff2)
          diff-num (count diffs)]
      (is (> 3 diff-num))
      (when (pos? diff-num)
        (is (contains? (set diffs) :you)))
      step2)))

(deftest test-river-crossing-plan
  (let [crossing-plan (map (partial map set) (river-crossing-plan))]
    (testing "you begin with the fox, goose and corn on one side of the river"
      (is (= [#{:you :fox :goose :corn} #{:boat} #{}]
             (first crossing-plan))))
    (testing "you end with the fox, goose and corn on one side of the river"
      (is (= [#{} #{:boat} #{:you :fox :goose :corn}]
             (last crossing-plan))))
    (testing "things are safe"
      (let [left-bank (map first crossing-plan)
            right-bank (map last crossing-plan)]
        (testing "the fox and the goose should never be left alone together"
          (is (empty?
               (filter #(= % #{:fox :goose}) (concat left-bank right-bank)))))
        (testing "the goose and the corn should never be left alone together"
          (is (empty?
               (filter #(= % #{:goose :corn}) (concat left-bank right-bank)))))))
    (testing "The boat can carry only you plus one other"
      (let [boat-positions (map second crossing-plan)]
        (is (empty?
             (filter #(> (count %) 3) boat-positions)))))
    (testing "moves are valid"
      (let [left-moves (map first crossing-plan)
            middle-moves (map second crossing-plan)
            right-moves (map last crossing-plan)]
        (reduce validate-move left-moves)
        (reduce validate-move middle-moves)
        (reduce validate-move right-moves )))))

