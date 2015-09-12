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
