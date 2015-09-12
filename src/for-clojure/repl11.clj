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

(fn reparent [e t]
  (->> t
       (tree-seq next rest)
       (filter #(some #{e} (flatten %)))
       (reduce (fn [a b]
                 (concat b (list (remove #{b} a)))))))