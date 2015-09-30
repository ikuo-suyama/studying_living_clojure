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

; ----------------#141
(let [notrump (__ nil)]
  (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                           {:suit :club :rank 9}]))
       (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                           {:suit :club :rank 10}]))))

(((fn [trump]
    (fn winner [tricks]
      (let [target (if (nil? trump)
                     tricks
                     (if-let [trumps (filter #(= (:suit %) trump) tricks)]
                       trumps
                       tricks))
            lead (:suit (first target))]
        (->> target
             (filter #(= (:suit %) lead))
             (sort-by #(:rank %))
             (last))))
    ) nil)
  [{:suit :spade :rank 2}
   {:suit :club :rank 10}])

; [Excellent]
(fn [t] (fn [C]
          (->> C
               (filter #(= (:suit %) (or t (:suit (first C)))))
               (apply max-key :rank))))


; ----------------#144
(= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
(take 4 ((fn [val & funcs]
    (let [inf-funcs (cycle funcs)
          lazy-vals (fn _lv [tar funcs]
                      (let [f (first funcs)
                            v (f tar)]
                        (lazy-seq (cons tar (_lv v (rest funcs))))))]
      (lazy-vals val inf-funcs)
      ))
   3.14 int double))

(= :gt (__ < 5 1))

((fn [f x y]
   (cond
     (true? (f y x)) :gt
     (true? (f x y)) :lt
     :else :eq
     )) (fn [x y] (< (count x) (count y))) "pear" "plum")

; ----------------#147
(= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])

(take 3 ((fn p [coll]
    (let [n (map +' (concat '(0) coll) (concat coll '(0)))]
      (lazy-cat (cons coll (p n))))
    ) [2 3 2]))

; ----------------#146
(= (__ '{a {p 1, q 2}
         b {m 3, n 4}})
   '{[a p] 1, [a q] 2
     [b m] 3, [b n] 4})

((fn [amap]
   (let [ks (keys amap)]
     (apply merge
            (flatten (for [k ks
                           :let [vs (get amap k)]]
                       (for [k2 (keys vs)]
                         {[k k2] (get vs k2)}
                         ))))))
  '{a {p 1, q 2}
    b {m 3, n 4}})

; ----------------#147
(= (__ #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)
(= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)
((fn [sets]
   (let [seqs (seq sets)
         tr (for [s seqs]
              (set (map #(if (coll? %)
                          (map str (set %))
                          (hash-set (str %))) s)))]
     (loop [c (first tr)
            t (rest tr)]
       (println c t)
       (if (some #(or (clojure.set/superset? c %) (clojure.set/superset? % c)) t)
         false
         (if (next t)
           (recur (first t) (rest t))
           true))
       )))
  #{#{(#(-> *)) + (quote mapcat) #_ nil}
    #{'+ '* mapcat (comment mapcat)}
    #{(do) set contains? nil?}
    #{, , , #_, , empty?}}
  )
; 全部シンボルにする？ Intelijでは通るが4clojrueで通らない
; Excellent
#(% distinct? (% concat %2)) apply
