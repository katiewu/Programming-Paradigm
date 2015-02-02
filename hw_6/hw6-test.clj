(ns user (:use clojure.test))
(load-file "hw6.clj")

(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(deftest test-suit
  (is (= (suit "2H") "H"))
  (is (= (suit "2D") "D"))
  (is (= (suit "2C") "C"))
  (is (= (suit "3S") "S")))

(deftest test-suit
  (is (= (rank "2H") 2))
  (is (= (rank "4S") 4))
  (is (= (rank "TS") 10))
  (is (= (rank "JS") 11))
  (is (= (rank "QS") 12))
  (is (= (rank "KS") 13))
  (is (= (rank "AS") 14)))

(deftest test-pair?
  (is (= (pair? pair-hand) true))
  (is (= (pair? high-seven) false)))

(deftest test-three-of-a-kind?
  (is (= (three-of-a-kind? two-pairs-hand) false))
  (is (= (three-of-a-kind? three-of-a-kind-hand) true)))

(deftest test-four-of-a-kind?
  (is (= (four-of-a-kind? two-pairs-hand) false))
  (is (= (four-of-a-kind? four-of-a-kind-hand) true)))

(deftest test-flush?
  (is (= (flush? pair-hand) false))
  (is (= (flush? flush-hand) true)))

(deftest test-full-house?
  (is (= (full-house? three-of-a-kind-hand) false))
  (is (= (full-house? full-house-hand) true)))

(deftest test-two-pairs?
  (is (= (two-pairs? two-pairs-hand) true))
  (is (= (two-pairs? pair-hand) false))
  (is (= (two-pairs? four-of-a-kind-hand) false)))

(deftest test-straight?
  (is (= (straight? two-pairs-hand) false))
  (is (= (straight? straight-hand) true))
  (is (= (straight? low-ace-straight-hand) true))
  (is (= (straight? ["2H" "2D" "3H" "4H" "5H"]) false))
  (is (= (straight? high-ace-straight-hand) true)))

(deftest test-straight-flush?
  (is (= (straight-flush? straight-hand) false))
  (is (= (straight-flush? flush-hand) false))
  (is (= (straight-flush? straight-flush-hand) true))
  (is (= (straight-flush? low-ace-straight-flush-hand) true))
  (is (= (straight-flush? high-ace-straight-flush-hand) true)))

(deftest test-value
  (is (= (value high-seven) 0))
  (is (= (value pair-hand) 1))
  (is (= (value two-pairs-hand) 2))
  (is (= (value three-of-a-kind-hand) 3))
  (is (= (value straight-hand) 4))
  (is (= (value flush-hand) 5))
  (is (= (value full-house-hand) 6))
  (is (= (value four-of-a-kind-hand) 7))
  (is (= (value straight-flush-hand) 8)))

(deftest test-kicker
  (is (= (kickers ["2H" "3S" "6C" "5D" "4D"]) '(6 5 4 3 2)))
  (is (= (kickers ["5H" "AD" "5C" "7D" "AS"]) '(14 5 7)))
  (is (= (kickers ["6H" "QD" "3D" "3S" "6H"]) '(6 3 12))))

(deftest test-higher-kicker?
  (is (= (higher-kicker? '(8 5 9) '(8 7 3)) false))
  (is (= (higher-kicker? '(8 7 5) '(8 7 5)) false))
	(is (= (higher-kicker? '(8 7 5) '(8 7 5 4 9)) false))
  (is (= (higher-kicker? '(8 7 5) '(2 3 2)) true)))

(deftest test-beats?
  (is (= (beats? three-of-a-kind-hand flush-hand) nil))
  (is (= (beats? full-house-hand high-seven) true))
  (is (= (beats? high-ace-straight-flush-hand low-ace-straight-flush-hand) true)))

(deftest test-winning-hand
  (is (= (winning-hand ) nil))
  (is (= (winning-hand ["2C" "3H" "7H" "5H" "AH"] ["2H" "3H" "4H" "5H" "AH"] ["2H" "2D" "3S" "KC"]
                       ["2H" "2C" "4S" "4D"] ["2H" "2D" "2S" "KC"] ["2H" "3H" "4S" "5H" "AH"] ["3H" "7H" "9H" "AH"]
                       ["2H" "2C" "2S" "3D" "3S" "5D"] ["2H" "2D" "2S" "2C" "5S"]) ["2H" "3H" "4H" "5H" "AH"])))



(run-tests)