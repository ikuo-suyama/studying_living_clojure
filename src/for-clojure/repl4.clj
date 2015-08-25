(ns for-clojure.repl4)

; ----------------#73
(= nil (__ [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))

(= :x (__ [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))

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


; ----------------#74
(= (__ "4,5,6,7,8,9") "4,9")
((fn is-perfect-sqrt? [str]
   (let [coll (-> str (.split ","))]
     (clojure.string/join "," (filter #(== (int (Math/sqrt (Integer/parseInt %))) (Math/sqrt (Integer/parseInt %))) coll))
     )) "4,5,6,7,8,9")

; [excellent]
(fn [s] (clojure.string/join ","
                             (filter (set (map #(str (* % %)) (range 9)))
                                     (re-seq #"\d+" s))))

; ----------------#75
; Euler's Totient Function
; count of gcd = 1 in less than f(x)
(= (__ 10) (count '(1 3 7 9)) 4)

((fn phi [x]
   (count (filter #(letfn [(gcd [a b]
                          (if (zero? a)
                            b
                            (gcd (mod b a) a)))]
              (= 1 (gcd x %)))
            (range 1 (inc x))))) 10)

((fn gcd [a b]
   (if (zero? a)
     b
     (gcd (mod b a) a))) 1 7)

; [excellent]
(fn [x] (count (filter #(= 1 ((fn g [a b] (if (zero? b) a (g b (mod a b)))) x %)) (range x))))


; ----------------#76
(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})

((fn [coll]
   (letfn [(ps [str]
               (-> str (.split "") (sort) (clojure.string/join)))]
     (reduce #(conj %1 (set %2)) #{}
       (filter #(< 1 (count %)) (partition-by ps (sort-by ps coll))))))
  ["meat" "mat" "team" "mate" "eat"])

; [excellent]
#(set (filter second (map set (vals (group-by sort %)))))
; (group-by sort ["meat" "mat" "team" "mate" "eat"])
; => {(\a \e \m \t) ["meat" "team" "mate"], (\a \m \t) ["mat"], (\a \e \t) ["eat"]}


; ----------------#78
(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop?(- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     ((fn mytr [func val]
        (loop [ret (func val)]
          (if (fn? ret)
            (recur (ret))
            ret))) triple 2))
   82)

; [excellent]
(fn t [f & a] (if (fn? f) (t (apply f a)) f))


; ----------------#79
(= 7 (__ '( [1]
           [2 4]
          [5 1 4]
         [2 3 4 5]))) ; 1->2->1->3

;0 1 3 6
;0 1 3 7
;0 1 4 7

((fn [seq]
   (letfn [(calc-sum [coll]
                     (map #(reduce + %) coll))

           (separate-pattern [coll depth]
                             (partition depth
                                        (flatten
                                          ((fn traversal [t coll]
                                             (if (nil? (second t))
                                               (conj coll (first t))
                                               (conj (traversal (second t) (conj coll (first t))) (traversal (last t) (conj coll (first t))))))
                                            coll []))))

           (mktree [coll pos]
                   (letfn [(tree
                             ([v l r] (conj '() r l v))
                             ([leaf] (conj '() nil nil leaf)))]
                     (if (next coll)
                       (tree
                         (get (first coll) pos)             ;val
                         (mktree (rest coll) pos)            ;left
                         (mktree (rest coll) (inc pos))      ;right
                         )
                       (tree (get (first coll) pos)))))
           ]

     (reduce min
             (calc-sum
               (separate-pattern
                 (mktree seq 0)
                 (count seq))))))

  '( [1]
     [2 4]
     [5 1 4]
     [2 3 4 5]))

;(defn tree
;  ([v l r] (conj '() r l v))
;  ([leaf] (conj '() nil nil leaf)))

; [excellent]
(fn g [[[f] & r]]
  (if r
    (+ f (min (g (map butlast r)) (g (map rest r))))
    f))

; 足し上げて小さい方を探索...!!
(fn nested [[[firstval] & restcoll]]
  (if restcoll
    (+ firstval
       (min
         (nested (map butlast restcoll))
         (nested (map rest restcoll))))
    firstval))


; ----------------#80
; A number is "perfect" if the sum of its divisors equal the number itself.
(= (__ 496) true)

((fn [v]
   (= v (reduce +
            (filter #(zero? (mod v %)) (range 1 v))))) 496)

; [excellent]
(#(= % (apply +
              (for [x (rest (range %)) :when (= (rem % x) 0)] x)
              )) 496)
;(for [x [0 1 2 3 4 5]
;      :let [y (* x 3)]
;      :when (even? y)]
;  y)
;;=> (0 6 12)

