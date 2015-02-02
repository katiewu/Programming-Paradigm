; author: Wu Jingyuan
; start the program: main(number, locations)
; output is trace of finding a better route and the final result.

(def info '((1 2) (-9 4) (3 5) (-6 3) (4 5) (9 -10) (3 8) (5 -2) (10 2) (0 2) (-11 -5) (-3 9)))
(def big '([0 0] [0 1] [0 2] [0 3] [0 4] [0 5] [0 6] [0 7] [1 0] [1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [2 0] [2 1] [2 2] [2 3] [2 4] [2 5] [2 6] [2 7] [3 0] [3 1] [3 2] [3 3] [3 4] [3 5] [3 6] [3 7] [4 0] [4 1] [4 2] [4 3] [4 4] [4 5] [4 6] [4 7] [5 0] [5 1] [5 2] [5 3] [5 4] [5 5] [5 6] [5 7] [6 0] [6 1] [6 2] [6 3] [6 4] [6 5] [6 6] [6 7] [7 0] [7 1] [7 2] [7 3] [7 4] [7 5] [7 6] [7 7]))
(def spliter (ref []))
(def maxroute '([(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]
                [(100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)]))
(def maxsubroute '((100 100) (100 -100) (-100 -100) (-100 100) (100 100) (100 -100) (-100 -100) (-100 100)))
(def locations (ref []))
(def optimalroute (ref []))
(def optimalvehicle (ref []))
(def who-found-it (ref :nobody))
(def agent1 (agent maxroute))
(def agent2 (agent maxroute))
(def agent3 (agent maxroute))
(def agent4 (agent maxroute))
(def agent5 (agent maxroute))
(def agent6 (agent maxroute))
(def agent7 (agent maxroute))
(def agent8 (agent maxroute))

;makeseqs: determine how many locations each vechile must visit
(defn makeseqs [n r result base]
  (cond (= n 0) result
        :else (cond (= r 0) (recur (- n 1) 0 (conj result base) base)
                    :else (recur (- n 1) (- r 1) (conj result (+ base 1)) base))))

;splitseqs: split the location, can be treated as an optimal route
(defn splitseqs [amb loc result]
  (cond (= (count amb) 0) result
        :else (recur (rest amb) (drop (first amb) loc) (conj result (take (first amb) loc)))))

(defn allocate [n loc]
  (let [r (rem (count loc) n) base (quot (count loc) n)]
    (let [s (makeseqs n r [] base)]
      (dosync (ref-set spliter s))
      (splitseqs s loc '()))))

(def initial (allocate n info))

(defn distance [location1 location2]
  (+ (Math/abs (- (first location2) (first location1))) (Math/abs (- (second location2) (second location1)))))

(defn computedistance [route sum]
  (cond (= (count route) 1) (+ sum (distance '(0 0) (first route)))
        :else (recur (drop 1 route) (+ sum (distance (first route) (second route))))))

(defn compute [route]
   (let [a (distance '(0 0) (first route))]
       (computedistance route a)))

(defn compareroute [route1 route2]
  (cond (<= (compute route1) (compute route2)) route1
        :else route2))

(defn update-vehicle [lst]
  (cond (empty? lst) @optimalvehicle
        :else (let [route (first lst)]
                (do
                  (dosync (alter optimalvehicle compareroute route))
                  (recur (rest lst))))))

(defn find-optimal-each-vehicle [vehicle-route]
  (dosync (ref-set optimalvehicle maxsubroute))
  (let [seqs (for [i (range 0 500)]
               (shuffle vehicle-route))]
    (update-vehicle seqs)))

(defn find-optimal-route [assign-route result]
  (cond (empty? assign-route) result
        :else (let [route (first assign-route)]
                (recur (rest assign-route) (concat result (list (find-optimal-each-vehicle route))))
                )))

(defn computeall [allroute result]
  (cond (empty? allroute) result
        :else (recur (rest allroute) (+ result (compute (first allroute))))))

(defn compareallroute [allroute1 allroute2 who]
  (cond (<= (computeall allroute1 0) (computeall allroute2 0)) allroute1
        :else (do
                (println who "found" allroute2)
                (ref-set who-found-it who)
                allroute2)))

;find the optimal solution for one shuffle
(defn update [allroute number who]
  (let [newroute (shuffle @locations)]
    (let [newassign (allocate number newroute)]
      (let [result (find-optimal-route newassign [])]
        (do
          (dosync
            (alter optimalroute compareallroute result who)
            ))))))

;(defn awaitall [agent-list]
;  (cond (= (count agent-list) 1) (await (first agent-list))
;        :else (do
;                (await (first agent-list))
;                (awaitall (rest agent-list)))))

;(defn main [allroute, number]
;  (dosync (ref-set optimalroute maxroute))
;  (dosync (ref-set locations allroute))
;  (let [agent-list (for [i (range 0 8)]
;                     (send (agent allroute) update number))]
;    (do
;      (awaitall agent-list)
;      @optimalroute)))

(defn main
  ([number allroute] (do
                       (dosync (ref-set optimalroute maxroute))
                       (dosync (ref-set locations allroute))
                       (main number allroute 10)))
  ([number allroute tries]
    (if (zero? tries)
      (do
        (await agent1 agent2 agent3 agent4 agent5 agent6 agent7 agent8)
        (println "best-solution" @who-found-it "found" @optimalroute))
      (do
        (send agent1 update number :agent1)
        (send agent2 update number :agent2)
        (send agent3 update number :agent3)
        (send agent4 update number :agent4)
        (send agent5 update number :agent5)
        (send agent6 update number :agent6)
        (send agent7 update number :agent7)
        (send agent8 update number :agent8)
        (recur number allroute (dec tries))))))







