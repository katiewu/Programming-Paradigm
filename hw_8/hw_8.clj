(defmacro for-loop [x init condition step & action]
  `(loop [~x ~init value# nil]
     (cond ~condition
           (let [new-value# (do ~@action)]
             ;(println (str "The result is " new-value#))
             (recur ~step new-value#))
           :else value#)))
  