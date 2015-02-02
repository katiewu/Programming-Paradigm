Author: Wu Jingyuan

example1:
(for-loop i 0 (< i 3) (inc i) (println i))
0
1
2
3
nil

example2:
(for-loop i 5 (> i 0) (dec i) (shuffle '(1 2 3 4)))
[1 4 3 2]

example3:
(macroexpand '(for-loop i 0 (< i 3) (inc i) (println i)))
(loop* [i 0 value__1881__auto__ nil] 
    (cond (< i 3) 
         (let [new-value__1882__auto__ (do (println i))] 
           (recur (inc i) new-value__1882__auto__)) 
          :else value__1881__auto__))