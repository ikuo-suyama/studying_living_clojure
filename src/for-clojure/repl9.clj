(ns for-clojure.repl9)

; ----------------#120
(= 2 ((__ '(/ a b))
       '{b 8 a 16}))

((fn [form val]
   (eval form)
   )
  '(/ a b)
  '{b 8 a 16})


(= 2 ((
        '(/ a b))
       '{b 8 a 16}))


; [excellent]

; ' ... 評価を一回遅延する
; '{} - MapEntry
; Symbol - Ver - Object
; Symbol と Verを結びつける → intern

((fn [syms]
   (let [_ns *ns*]
     (doseq [sym syms]
       (intern _ns (key sym) (val sym))))
   ;なんか近い
   ) '{x 4 y 1})

; '() -> evalならOK
; () -> compile error
; 関数実行時に、コンパイルエラーだから？
; do
; (do x y z) compiles x, runs x; compiles y, runs y; compiles z, runs z.
(((fn [form]
    (fn [syms]
      (do
        (doseq [sym syms]
          (intern *ns* (key sym) (val sym)))
        (eval form)))) '(/ a b))
  '{b 8 a 16})
; -> できるけど internはtrap

(((fn [form]
    (fn [syms]
      (do
        (doseq [sym syms]
          (eval (list 'def (key sym) (val sym))))
        (eval form)))) '(/ a b))
  '{b 8 a 16})
; eval, resolve も NG...

; read-string に #=としたリストを食わせれば評価される
(fn [form]
  (fn [params]
    (read-string (str "#=(inc 1)"))))
; が、java.lang.RuntimeException: EvalReader not allowed when *read-eval* is false.

; どうも、formを読んで新しく式を返させたいよう
(((fn [form]
    (fn [params]
      (let [TABLE (conj {'+ +
                         '- -
                         '/ /
                         '* *} params)

            exec (fn [f & args]
                   (apply f args))]

        (letfn [(ev [_form]
                    (apply exec
                           (for [x _form]
                             (cond
                               (= clojure.lang.Symbol (type x)) (get TABLE x)
                               (coll? x) (ev x)
                               :else x
                               ))))]
          (ev form)
        ))))
   '(* (+ 2 a)
       (- 10 b)))
  '{b 8 a 16})



;(#'studing-living-clojure.core/x #'studing-living-clojure.core/y)
;-> Var

; ----------------#122
(= 7     (__ "111"))

((fn [nx2]
   (int (reduce +
            (map-indexed #(* (Math/pow 2 %) (Integer/parseInt (str %2)))
                         (reverse (seq nx2)))))) "0")

; [excellent]
#(Long/parseLong % 2)

(= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (__ '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))

((fn [board color]
   (let [MAXL (count board)
         MAXC (count (first board))
         COMP ({'b 'w 'w 'b} color)]
     (letfn [(empties []
                      (for [i (range MAXL)
                            j (range MAXC)
                            :when (= 'e ((board i) j))]
                        [i j]))

             (flipped-coor [coll]
                           (let [colors (for [coor coll
                                              :let [x (first coor)
                                                    y (second coor)]
                                              :while (or (= [x y] (first coll))
                                                         (not= ((board x) y) 'e))
                                              ]
                                          ((board x) y))]
                             (cond
                               (> 3 (count colors)) nil
                               (= color (last colors))
                               (set (take (count (filter #(= COMP %) colors)) (rest coll)))
                               )))

             (flipped [coor]
                      (let [x (first coor)
                            y (second coor)
                            flipped-coors
                            (apply clojure.set/union #{}
                                   (map flipped-coor
                                        (conj []
                                              ; line
                                              (for [j (range y MAXC)] [x j])
                                              (for [j (reverse (range (inc y)))] [x j])
                                              ; coll
                                              (for [i (range x MAXL)] [i y])
                                              (for [i (reverse (range (inc x)))] [i y])
                                              ; cross
                                              (for [i (range MAXC)
                                                    :let [x' (+ x i)
                                                          y' (+ y i)]
                                                    :while (and (< x' MAXL)
                                                                (< y' MAXC))] [x' y'])
                                              (for [i (range MAXC)
                                                    :let [x' (- x i)
                                                          y' (- y i)]
                                                    :while (and (>= x' 0)
                                                                (>= y' 0))] [x' y'])
                                              (for [i (range MAXC)
                                                    :let [x' (+ x i)
                                                          y' (- y i)]
                                                    :while (and (< x' MAXL)
                                                                (>= y' 0))] [x' y'])
                                              (for [i (range MAXC)
                                                    :let [x' (- x i)
                                                          y' (+ y i)]
                                                    :while (and (>= x' 0)
                                                                (< y' MAXC))] [x' y'])
                                              )))]

                        (if (not-empty flipped-coors)
                          [coor flipped-coors]
                          nil)

                        ))
             ]
       (reduce #(conj %1 (apply hash-map %2)) {}
         (filter (complement nil?)
                 (for [c (empties)]
                   (flipped c)))))))
  '[[e e e e]
    [e w b e]
    [w w w e]
    [e e e e]] 'b
  )

;(apply hash-map (flatten '((:a :b) (:c :d))))

(
  ;(str '
  (fn Quines []
   (let [q  (char 34)
         lf (char 32)
         st [
             "(fn Quines []"
             "(let [q (char 34)"
             "lf (char 32)"
             "st ["
             "]]"
             "(str"
             "(clojure.string/trim (apply str (for [s (range 4)] (str (st s) lf))))"
             "(clojure.string/trim (apply str (for [s st] (str q s q lf))))"
             "(clojure.string/trim (apply str (for [s (range 4 (count st))] (str (st s) lf)))))))"
             ]]
     (str
       (clojure.string/trim (apply str (for [s (range 4)] (str (st s) lf))))
       (clojure.string/trim (apply str (for [s st] (str q s q lf))))
       (clojure.string/trim (apply str (for [s (range 4 (count st))] (str (st s) lf)))))))
  )

; [excellent]
((fn [] (let [s "(fn [] (let [s %s] (format s (pr-str s))))"] (format s (pr-str s)))))
(fn [p] (str p p)) '(fn [p] (str p p))
