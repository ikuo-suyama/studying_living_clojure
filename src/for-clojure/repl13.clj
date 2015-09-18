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
   (let [num (count (first sets))
         sym->fn (fn [sym]
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
                           (reduce bit-or)))))

         hum-dis1? (fn [a b]
                   (let [hum-dis (bit-xor
                                   (Integer/parseInt (apply str a) 2)
                                   (Integer/parseInt (apply str b) 2))]
                     (some identity
                          (for [i (range num)]
                            (= hum-dis (bit-shift-left 1 i))
                            ))))

         graycodes (fn [n]
                     (->>
                       (for [i (range (Math/pow n 2))
                             :let [_i (bit-shift-right i 1)]]
                         (bit-xor i _i))
                       (map #(format (str "%0" num "d") (Integer/parseInt (Integer/toBinaryString %))))
                       (map #(map (fn [n] (Integer/parseInt (str n))) (seq %)))
                       ))

         scan-rect (fn [trues]
                     ; ハミング距離が１(loop)、Sizeが 2^n

                     )
         ]

     (let [af (create-af sets)]
       (->> (for [b (graycodes num)]
              [b (af b)])
            (filter #(= 1 (second %)))
            ))
     (hum-dis1? '(1 1 0 0) '(1 1 1 1))
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
[(0 1 1 0) 1]

[(1 1 0 0) 1]
[(1 1 0 1) 1]

[(1 1 1 0) 1]
[(1 0 1 0) 1]
[(1 0 1 1) 1]
[(1 0 0 1) 1]
[(1 0 0 0) 1]
