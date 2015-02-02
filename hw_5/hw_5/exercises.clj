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