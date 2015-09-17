(ns for-clojure.repl11)

; ----------------#128
;(= {:suit :diamond :rank 10} (__ "DQ"))
((fn translate [str]
   (let [SUIT {\S :spade
               \H :heart
               \D :diamond
               \C :club}
         RANK (apply assoc {}
                (interleave [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]
                           (range 13)
                           ))]
     (let [strv (seq str)]
       {:suit (SUIT (first strv)) :rank (RANK (last strv))})
     )) "DQ")


; ----------------#130
(= '(a (t (e)))
   (__ 'a '(t (e) (a))))

(= '(d
      (b
        (c)
        (e)
        (a
          (f
            (g)
            (h)))))
   (__ 'd '(a
             (b
               (c)
               (d)
               (e))
             (f
               (g)
               (h)))))

((fn pikc-tree [new-root tree]
   (let [walked (tree-seq next next tree)
         tree-reverse
         (fn _tr [before self current]
           (println "self: " self "current:" current)
           (if (empty? current)
             self
             (if (some #(= self %) (first current))
               (let [_self (first current)
                     _ret (remove #(= self %) _self)
                     _bef (remove #(= before %) self)]
                 (println "_self: " _self "_ret" _ret)
                 (if (next current)
                   (concat self (list (_tr self _self (rest current))))
                   (concat _bef (list _ret))
                   ))
               (recur self self (rest current)))
             ))]
     (let [me (first (filter #(= new-root (first %)) walked))
           target (take-while #(not= new-root (first %)) walked)]
       (tree-reverse
         me
         me
         (reverse target)))
     ))
  'a '(t (e) (a))
  ;'e '(a (t (e)))
  ; 'd '(a (b (c) (d) (e)) (f (g) (h)))
  ;'c '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))))
  )

; [excellent]
(fn reparent [e t]
  (->> t
       (tree-seq next rest)
       (filter #(some #{e} (flatten %)))
       (reduce (fn [a b]
                 (concat b (list (remove #{b} a)))))))

; ----------------#131
(= true  (__ #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))

((fn [& sets]
   (let [comb (fn _comb [t]
                (reduce
                  #(apply conj
                          (conj %1 #{%2})
                          (for [x %1] (conj x %2)))
                  #{#{(first t)}}
                  (rest t)))]

     ((complement empty?)
       (->> (map comb sets)
            (map (fn [_t] (set (map #(reduce + %) _t))))
            (apply clojure.set/intersection)))))

  #{-1 1 99}
  #{-2 2 888}
  #{-3 3 7777})


; ----------------#132
(= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))

((fn [f k coll]
   (filter #(not (nil? %))
     (reduce #(if (f (last %1) %2)
              (concat %1 (list k %2))
              (concat %1 (list %2))) (list (first coll)) (rest coll))))
  > :more ())

((fn spacer [f k coll]
   (if (empty? coll)
     nil
     (if (next coll)
       (if (f (first coll) (second coll))
         (lazy-seq (concat (list (first coll) k) (spacer f k (rest coll))))
         (lazy-seq (concat (list (first coll)) (spacer f k (rest coll))))
         )
       (list (last coll))
       ))
   ) > :more ())


(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first) ; fibonacci numbers
                 (
                   (fn spacer [f k coll]
                    (if (empty? coll)
                      nil
                      (if (next coll)
                        (if (f (first coll) (second coll))
                          (lazy-seq (concat (list (first coll) k) (spacer f k (rest coll))))
                          (lazy-seq (concat (list (first coll)) (spacer f k (rest coll))))
                          )
                        (list (last coll))
                        ))
                    )
                   (fn [a b] ; both even or both odd
                       (= (mod a 2) (mod b 2)))
                     :same))))

; [excellent]
; take 10してlazyを無効化
#(let [v (take 10 %3)
       o first]
  (if (>= 1 (count v)) v
                       (loop [r [(o v)]
                              f (drop-last v)
                              l (rest v)]
                         (if (= 0 (count f)) r
                                             (recur
                                               (if (% (o f) (o l)) (concat r [%2 (o l)]) (concat r [(o l)]))
                                               (rest f) (rest l))))))

; ----------------#135
(= 8  (__ 10 / 2 - 1 * 2))
((fn [& args]
   (reduce #((first %2) %1 (second %2)) (first args) (partition-all 2 (rest args))))
  10 / 2 - 1 * 2)

; [excellent]
(fn infix
  ([a op b] (op a b))
  ([a op b & more] (apply infix (op a b) more)))

; ----------------#135
(= [1 0 0 1] (__ 9 2))
((fn base [n b]
   (loop [_n n
          ret '()]
     (if (< _n b)
       (conj ret (mod _n b))
       (recur (quot _n b) (conj ret (mod _n b)))
       )))
  9 2)