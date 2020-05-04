(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

;; fill in  tests for your game
(deftest test-play-round
  (testing "equal values result in a draw"
    (is (= 0 (play-round [:heart 5] [:diamond 5]))))
  (testing "the highest rank wins the cards in the round"
    (is (= -1 (play-round [:spade 3] [:club 8])))
    (is (= 1 (play-round [:heart 7] [:spade 2]))))
  (testing "queens are higher rank than jacks"
    (is (= -1 (play-round [:heart :jack] [:club :queen])))
    (is (= 1 (play-round [:heart :queen] [:club :jack]))))
  (testing "kings are higher rank than queens"
    (is (= -1 (play-round [:spade :queen] [:club :king])))
    (is (= 1 (play-round [:spade :king] [:club :queen]))))
  (testing "aces are higher rank than kings")
    (is (= -1 (play-round [:club :king] [:club :ace]))
    (is (= 1 (play-round [:club :ace] [:club :king])))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (= [[] [[:club :queen][:club :ace]]]
           (play-game [[:club :queen]] [[:club :ace]])))
    (is (= [[[:club :queen][:club :ace]] []]
           (play-game [[:club :ace]] [[:club :queen]]))))
  (testing "a draw means war"
    (let [p1 [[:club 4]
              [:club 1]
              [:club 2]
              [:club 3]
              [:club :ace]]
          p2 [[:spade 4]
              [:spade 1]
              [:spade 2]
              [:spade 3]
              [:spade :jack]]]
      (is (= [[[:club 4]
               [:club 1]
               [:club 2]
               [:club 3]
               [:spade 4]
               [:spade 1]
               [:spade 2]
               [:spade 3]
               [:spade :jack]
               [:club :ace]]
              []]
             (play-game p1 p2))))))
