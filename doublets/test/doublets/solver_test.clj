(ns doublets.solver-test
  (:require [clojure.test :refer :all]
            [doublets.solver :refer :all]))

(deftest dist-test
  (testing "simple distance between two words"
    (is (= 0 (dist "book" "book")))
    (is (= 1 (dist "book" "look")))
    (is (= 2 (dist "head" "teal")))
    (is (= 4 (dist "bank" "loan")))))

(deftest candidates-test
  (testing "it finds candidates for words"
    (is (= '("heal") (candidates "head" [])))))

(deftest solver-test
  (testing "with word links found"
    (is (= ["head" "heal" "teal" "tell" "tall" "tail"]
           (doublets "head" "tail")))

    (is (= ["door" "boor" "book" "look" "lock"]
           (doublets "door" "lock")))

    (is (= ["bank" "bonk" "book" "look" "loon" "loan"]
           (doublets "bank" "loan")))

    (is (= ["wheat" "cheat" "cheap" "cheep" "creep" "creed" "breed" "bread"]
           (doublets "wheat" "bread"))))

  (testing "with no word links found"
    (is (= []
           (doublets "ye" "freezer")))))
