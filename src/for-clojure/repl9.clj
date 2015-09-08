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