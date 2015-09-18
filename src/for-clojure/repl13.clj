(ns for-clojure.repl13)

(= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c}
     #{'A 'b}
     #{'B 'C 'd}})

((fn [sets]
   (let [sym->fn (fn [sym]
                   (if (= (str sym) (clojure.string/upper-case (str sym)))
                     identity
                     #(bit-flip % 0)))
         create-af (fn [_sets]
                     (let [checker (map #(->> (sort-by (fn [_s]
                                                         (clojure.string/lower-case (str _s))) %)
                                              (map sym->fn))
                                        _sets)]
                       (fn [bits]
                         (->>
                           (map
                             #(->> (map (fn [f b] (f b)) % bits)
                                   (reduce bit-and))
                             checker)
                           (reduce bit-or))
                         )))
         graycodes (fn [n]
                     (for [i (range (Math/pow n 2))
                           :let [_i (bit-shift-right i 1)]]
                       (bit-xor i _i)))
         ]

     ((create-af sets) [1 0 1 1])
     (map #(Integer/toBinaryString %) (graycodes 4))

     ))
  #{#{'a 'B 'C 'd}
    #{'A 'b 'c 'd}
    #{'A 'b 'c 'D}
    #{'A 'b 'C 'd}
    #{'A 'b 'C 'D}
    #{'A 'B 'c 'd}
    #{'A 'B 'c 'D}
    #{'A 'B 'C 'd}})

; Gray Code
0000
0001 1
0011 1
0010
0110
0111 1
0101 1
0100
1100 1
1101 1
1111
1110
1010
1011
1001
1000

; 1 だけ残す、
; ハミング距離が１の連続の組み合わせを取る
; 符号化する
(let [checker (map #(map sym->fn
                         (sort-by (fn [_s]
                                    (clojure.string/lower-case (str _s))) %))
                   _sets)]
