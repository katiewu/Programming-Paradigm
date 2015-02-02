(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn rank [card]
  (let [[fst _] card]
    (if (java.lang.Character/isDigit fst)
      (java.lang.Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn check [num]
  (fn [hand] (= (apply max (vals (frequencies (map rank hand)))) num)))

(def pair? (check 2))

(def three-of-a-kind? (check 3))

(def four-of-a-kind? (check 4))

(defn flush? [hand]
  (let [seqs (vals (frequencies (map suit hand)))]
    (cond (.contains seqs 5) true
          :else false)))

(defn full-house? [hand]
  (let [seqs (vals (frequencies (map rank hand)))]
    (and (.contains seqs 2) (.contains seqs 3))))

(defn two-pairs? [hand]
  (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [seqs (sort (map rank hand))]
    (or (= seqs '(2 3 4 5 14)) (= seqs (range (apply min seqs) (+ (apply min seqs) 5))))))

(defn straight-flush? [hand]
  (let [seqs (vals (frequencies (map suit hand)))]
    (cond (.contains seqs 5) (straight? hand)
          :else false)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

(defn kickers [hand]
  (let [seqs (map key (sort-by val > (sort-by key > (frequencies (map rank hand)))))]
    (cond (= seqs '(14 5 4 3 2)) '(5 4 3 2 1)
          :else seqs)))


(defn higher-kicker? [kicker1 kicker2]
  (cond (empty? kicker1) false
        :else (let [val (compare (first kicker1) (first kicker2))]
                (cond (= val -1) false
                      (= val 0) (higher-kicker? (rest kicker1) (rest kicker2))
                      :else true))))

(defn beats? [hand1 hand2]
  (cond (> (value hand1) (value hand2)) true
        (= (value hand1) (value hand2)) (cond (higher-kicker? (kickers hand1) (kickers hand2)) true
                                              :else nil)
        :else nil))

(defn equal [hand1 hand2]
  (cond (= (value hand1) (value hand2)) (cond (= (kickers hand1) (kickers hand2)) true
                                              :else false)
           :else false))

(defn winning
  ([hands] (winning (list (first hands)) (rest hands)))
  ([results hands] (cond (empty? hands) (cond (= 1 (count results)) (first results)
                                              :else results)
                       :else (cond (beats? (first results) (first hands)) (recur results (rest hands))
                                   (equal (first results) (first hands)) (recur (cons (first hands) results) (rest hands))
                                   :else (recur (list (first hands)) (rest hands))))))

(defn winning-hand [& hands]
  (cond (empty? hands) nil
        :else (winning hands)))