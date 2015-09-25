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

         pick-hum1 (fn [current target]
                     (reduce
                       (fn [ret cur]
                         (if (hum-dis1? (last ret) cur)
                           (concat ret (list cur))
                           ret))
                       (list current)
                       target))

         pick-rect (fn [current target]
                     (if (next target)
                       (loop [_t (reverse target)]
                         (if (hum-dis1? current (first _t))
                           (reverse _t)
                           (recur (rest _t))))
                       nil))

         scan-rect (fn [trues]
                     ; ハミング距離が１(loop)、Sizeが 2^n
                     (loop [ret '()
                            target (rest trues)
                            current (first trues)]
                       (let [hum1s (pick-hum1 current target)
                             rect (pick-rect current hum1s)]
                         (println "hum1: " hum1s)
                         (println "rect: " rect)
                         (if (empty? target)
                           (cons rect ret)
                           (recur (cons rect ret) (rest target) (first target))))))

         dup? (fn [check all]
                (let [_all (set (apply concat all))]
                  (println "check: " check "all: " _all)
                  (every? #(contains? _all %) check)))

         reduce-dup (fn [rects]
                      (println "rects:" rects)
                      (loop [ret '()
                             cur (first rects)
                             tar (rest rects)]
                        (if (empty? tar)
                          (if (dup? cur (concat tar ret))
                            ret
                            (cons cur ret))
                          (if (dup? cur (concat tar ret))
                            (recur ret (first tar) (rest tar))
                            (recur (cons cur ret) (first tar) (rest tar))))))

         ALPHA {1 ['A 'B 'C 'D] 0 ['a 'b 'c 'd]}

         bin->alph (fn [rect]
                     (let [alphas (for [_set rect
                                        :let [v (vec _set)]]
                                    (for [i (range (count v))]
                                      ((get ALPHA (v i)) i))
                                    )]
                       (->> (flatten alphas)
                            (group-by identity)
                            (filter #(= (count alphas) (count (second %))))
                            (map first)
                            (set)
                            )))

         translate (fn [rects]
                     (set
                       (for [rect rects]
                         (bin->alph rect))))
         ]

     (let [af (create-af sets)
           ret
           (->> (for [b (graycodes num)]
                  [b (af b)])
                (filter #(= 1 (second %)))
                ;([(0 1 1 0) 1] ...)
                (map first)
                (scan-rect)
                (sort-by count)
                (reduce-dup)
                (translate))]
       (if (empty? ret)
         sets
         ret)
     )))
  #{#{'a 'B 'c 'd}
    #{'A 'B 'c 'D}
    #{'A 'b 'C 'D}
    #{'a 'b 'c 'D}
    #{'a 'B 'C 'D}
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
