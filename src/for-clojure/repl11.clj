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
         (fn _tr [self current]
           (println "self: " self "current:" current)
           (if (empty? current)
             self
             (if (some #(= self %) (first current))
               (let [_self (first current)
                     _ret (remove #(= self %) _self)]
                 (println "_self: " _self "_ret" _ret)
                 (if (next current)
                   (concat self (list (_tr _self (rest current))))
                   (concat self (list _ret))
                   ))
               (recur self (rest current)))
             ))]
     (tree-reverse (list new-root)
                   (reverse (take-while #(not= new-root (first %)) walked)))
     ))
  ;'a '(t (e) (a))
  ; 'e '(a (t (e))
  'd '(a (b (c) (d) (e)) (f (g) (h)))
  )

;(loop [self (list new-root)
;       ret  (list new-root)
;       current (reverse (take-while #(not= new-root (first %)) walked))
;       ]
;  (if (empty? current)
;    ret
;    (if (some #(= self %) (first current))
;      (let [_self (first current)
;            _ret  (concat ret (list (remove #(= self %) _self)))]
;        (recur _self _ret (rest current)))
;      (recur self ret (rest current)))
;    )))

(tree-seq next next '(a (t (e))))
(reverse (take-while #(not= 'e (first %)) '(a (t (e)))))

(tree-seq next next '(a
                       (b
                         (c)
                         (d)
                         (e))
                       (f
                         (g)
                         (h))))
'((a (b (c) (d) (e)) (f (g) (h)))
   (b (c) (d) (e))
   (c)
   (d)
   (e)
   (f (g) (h))
   (g)
   (h))


(= '(e (t (a)))
   (__ 'e '(a (t (e)))))