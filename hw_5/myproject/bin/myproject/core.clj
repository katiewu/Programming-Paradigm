(ns myproject.core  (:use clojure.test))

(defn collatz [n]
  (if (= n 1)
    '(1)
    (if (even? n)
      (conj (collatz (/ n 2)) n)
      (conj (collatz (inc (* 3 n))) n))))

(defn shallow-reverse [lst]
  (loop [ col lst result '()]
    (if (empty? col)
      result
      (recur (rest col) (cons (first col) result)))))
 
(defn remove-duplicates [lst]
  (cond (empty? lst)
        '()
        :else (cond (empty? (rest lst))
                    (list (first lst))
                    :else (cons (first lst) (remove-duplicates (filter (fn [x] (not= x (first lst))) (rest lst)))))))
    
(defn my-flatten [lst]
 (if (empty? lst)
   '()
   (if (list? (first lst))
     (concat (my-flatten (first lst)) (my-flatten (rest lst)))
     (conj (my-flatten (rest lst)) (first lst)))))

(defn skeleton [lst]
  (if (empty? lst)
    '()
    (if (list? (first lst))
      (concat (list (skeleton(first lst))) (skeleton (rest lst)))
      (skeleton (rest lst)))))

(defn deep-reverse [lst]
  (loop [lst lst result '()]
    (if (empty? lst)
      result
      (if (list? (first lst))
        (recur (rest lst) (concat (list (deep-reverse (first lst))) result))
        (recur (rest lst) (cons (first lst) result))))))

(defn eliminate [value lst]
  (if (empty? lst)
    '()
    (if (= (first lst) value)
      (eliminate value (rest lst))
      (if (list? (first lst))
        (concat (list (eliminate value (first lst))) (eliminate value (rest lst)))
        (conj (eliminate value (rest lst)) (first lst))))))

(defn quicksort [lst]
  (if (empty? lst)
    '()
    (concat (concat (quicksort (filter (fn [x] (< x (first lst))) lst)) (filter #(= (first lst) %) lst)) (quicksort (filter (fn [x] (> x (first lst))) lst)))))

(defn zap-gremlins [text]
  (filter (fn [x] (or (and (>= (int x) 32) (<= (int x) 126)) (= (int x) 10) (= (int x) 13))) text))
         
(defn rot-13 [text]
  (map (fn [x] (if (and (>= (int x) 65) (<= (int x) 90))
                 (char (+ (rem (+ (- (int x) 65) 13) 26) 65))
                 (if (and (>= (int x) 97) (<= (int x) 122))
                   (char (+ (rem (+ (- (int x) 97) 13) 26) 97))
                   x))) text))

(defn absol [x]
  (cond (pos? x) x
        :else (- 0 x)))

(defn my-sqrt 
  ([n] (my-sqrt 2 n))
  ([r n] (cond (< (absol (- n (* r r))) 0.00001) (float r)
                         :else (my-sqrt (/ (+ r (/ n r)) 2) n))))

(defn longest-collatz [lo hi]
  (cond (= lo hi)
        lo
        :else (cond (> (count (collatz lo)) (count (collatz hi)))
                    (longest-collatz lo (- hi 1))
                    :else (longest-collatz (+ lo 1) hi))))

(deftest test-collatz
  (is (= '(1) (collatz 1)))
  (is (= '(10 5 16 8 4 2 1) (collatz 10)))
  (is (= '(17 52 26 13 40 20 10 5 16 8 4 2 1) (collatz 17)))
  (is (= '(23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1) (collatz 23))))

(deftest test-shallow-reverse
  (is (= '() (shallow-reverse '())))
  (is (= '(5 4 3 2 1) (shallow-reverse '(1 2 3 4 5))))
  (is (= '(5 ((3) 4) 2 1) (shallow-reverse '(1 2 ((3) 4) 5))))
  (is (= '(5 (1 2 ((3) 4))) (shallow-reverse '((1 2 ((3) 4)) 5))))
  (is (= '((())) (shallow-reverse '((())))))
)

(deftest test-remove-duplicates
  (is (= '(1 2 3 4 5) (remove-duplicates '(1 2 3 4 5))))
  (is (or (= '(1 2 ((3 4)) 5 ()) (remove-duplicates '(1 2 1 ((3 4)) 5 ())))
          (= '(2 1 ((3 4)) 5 ())
             (remove-duplicates '(1 2 1 ((3 4)) 5 ())))))
  (is (= '(1) (remove-duplicates '(1 1 1))))
  (is (= '(1 (1)) (remove-duplicates '(1 1 1 (1)))))
)

(deftest test-my-flatten
  (is '() (my-flatten '(() () ((())))))
  (is (= '(1) (my-flatten '(() (() () 1) (())))))
  (is (= '(1 2 3 4 5) (my-flatten '(1 (2) (3 ((4) (5)))))))
  (is (= '(1 2 3 4 5) (my-flatten '(1 2 3 4 5))))
)

(deftest test-skeleton
  (is (= '() (skeleton '())))
  (is (= '((()) () ()) (skeleton '(1 (2 (3 4)) 5 6 (7) ()))))
  (is (= '((())) (skeleton '(((1 2 3) 4 2) 9 8)))))

(deftest test-deep-reverse
  (is (= '() (deep-reverse '())))
  (is (= '(((1) 3)) (deep-reverse '((3 (1))))))
  (is (= '(5 4 3 2 1) (deep-reverse '(1 2 3 4 5))))
  (is (= '((5) (((4)) 3) 2 1) (deep-reverse '(1 2 (3 ((4))) (5)))))
)

(deftest test-eliminate
  (is (= '() (eliminate 1 '())))
  (is (= '(2 3 4 5) (eliminate 1 '(1 2 3 4 5 1))))
  (is (= '(() (())) (eliminate 1 '(1 (1) ((1))))))
  (is (= '(() ((2))) (eliminate 1 '(1 (1) ((1 2))))))
  (is (= '(1 2 3 4 5 1) (eliminate 6 '(1 2 3 4 5 1))))
  (is (= '(()()) (eliminate 2 '(2 (2) ()))))
  (is (= '(1 2 3) (eliminate 5 '(5 1 5 2 5 5 3))))
)

(deftest test-quicksort
  (is (= '(2 2 3 4 5 5 23 43 67) (quicksort '(23 2 4 2 3 5 43 5 67))))
  (is (= '(4 8 10 13 28 30 48 69 94) (quicksort '(10 28 69 4 94 13 30 8 48))))
  (is (= '(1 2 3 4 5 6 7 8 9) (quicksort '(3 4 2 7 6 9 1 5 8)))))

(deftest test-zap-gremlins
  (is (or (= '(\a \b \1 \2 \~) (zap-gremlins '(\a \b \1 \2 3 4 \~ \backspace \tab)))
          (= '"ab12~" (zap-gremlins '(\a \b \1 \2 3 4 \~ \backspace \tab))) ))
    (is (or (= '(\h \i \newline \M \r \. \A \~ \B \C)
               (zap-gremlins '(\h \i \newline \M \r \. \tab \A \~ \B \C)))
            (= '"hi\nMr.A~BC" (zap-gremlins '(\h \i \newline \M \r \. \tab \A \~ \B \C))) ))
  (is (or (= () (zap-gremlins ())) (= "" (zap-gremlins ())) ))
)


(deftest test-rot-13
  (is (or (= '(\* \U \r \Y \y \B \space \$ \Z \l \@ \space \J \b \E \y \Q \*)
             (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*)))
          (= '"*UrYyB $Zl@ JbEyQ*"
             (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))) ))
  (is (or (= '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*)
             (rot-13 (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))))
          (= '"*HeLlO $My@ WoRlD*"
             (rot-13 (rot-13 '(\* \H \e \L \l \O \space \$ \M \y \@ \space \W \o \R \l \D \*))) )))
  (is (or (= () (rot-13 ())) (= "" (rot-13 ())) ))
  (is (or (= "NOPQRSTUVWXYZABCDEFGHIJKLM@nopqrstuvwxyzabcdefghijklm!"
             (rot-13 '(\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z \@ \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \!)))
          (= '(\N \O \P \Q \R \S \T \U \V \W \X \Y \Z \A \B \C \D \E \F \G \H \I \J \K \L \M \@ \n \o \p \q \r \s \t \u \v \w \x \y \z \a \b \c \d \e \f \g \h \i \j \k \l \m \!)
             (rot-13 '(\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z \@ \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z \!)))))
)

(def epsilon 0.00015)

(deftest test-sqrt
  (is (<= (absol (- 2 (my-sqrt 4))) epsilon))
  (is (<= (absol (- 1.732050808 (my-sqrt 3))) epsilon))
  (is (<= (absol (- 12 (my-sqrt 144))) epsilon))
  (is (<= (absol (- 35.1283361405006 (my-sqrt 1234))) epsilon))
)

(deftest test-longest-collatz
  (is (= 7 (longest-collatz 5 7)))
  (is (= 27 (longest-collatz 1 50)))
  (is (= 50 (longest-collatz 50 50)))
  (is (= 9 (longest-collatz 1 15)))
  (is (= 9 (longest-collatz 1 9)) "Boundary testing")
  (is (= 9 (longest-collatz 9 15)) "Boundary testing")
  (is (= 9 (longest-collatz 1 10)))
  (is (= 27 (longest-collatz 10 30)))
  (is (= 871 (longest-collatz 730 917)))
  (is (= 129 (longest-collatz 1 129)))
  (is (= 73 (longest-collatz 1 73)))
  (is (= 1161 (longest-collatz 1 1161)))
)

(run-tests)


  


         
         
         

