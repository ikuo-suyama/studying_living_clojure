(ns for-clojure.repl4)

; ----------------#66

(= nil (__ [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))

(= :x (__ [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))

[1 2 3]

((fn winner [coll]
  (let [x (-> coll (distinct))]
    (cond
      (= 1 (count x)) (first x)
      :else nil))) [:x :x :x])


((fn checker [bord]
   (let [flat-bord (reduce conj [] (flatten bord))]
     (letfn [(_g [inx] (get flat-bord inx))
             ; check the winner of each row
             (winner [coll]
                     (let [x (-> coll (distinct))]
                       (cond
                         (= 1 (count x)) (first x)
                         :else nil)))]
       ;convert every check pattern of row
       (loop [_target (map #(conj [] (_g (first %)) (_g (second %)) (_g (last %)))
                           [[0 1 2] [3 4 5] [6 7 8]
                            [0 3 6] [1 4 7] [2 5 8]
                            [0 4 8] [2 4 6]])]
         (when-not (empty? _target)
           (let [answer (winner (first _target))]
             (if (or (nil? answer) (= :e answer))
               (recur (rest _target))
               answer)))
         ))))
  [[:x :e :o]
   [:x :o :e]
   [:o :e :x]])


; [excellent]
(fn checkboard [board]
  (let [[[a11 a12 a13] [a21 a22 a23] [a31 a32 a33]] board
        won (into board [[a11 a21 a31] [a12 a22 a32] [a13 a23 a33] [a11 a22 a33] [a31 a22 a13]])]
    (if (seq (filter #(= % [:x :x :x]) won)) :x
     (if (seq (filter #(= % [:o :o :o]) won)) :o nil))))


