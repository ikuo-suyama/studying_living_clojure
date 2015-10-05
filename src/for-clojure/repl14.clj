(ns for-clojure.repl14)

(= 10 (((fn [f]
          (fn [& args]
            (loop [v (f (first args))
                   a (rest args)]
              (if (fn? v)
                (recur (v (first a)) (rest a))))))
         (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d)))))
         )
        1 2 3 4))

(((fn [f]
    (fn [& args]
      (loop [v (f (first args))
             a (rest args)]
        (if (fn? v)
          (recur (v (first a)) (rest a))
          v))))
   (fn [a]
     (fn [b]
       (fn [c]
         (fn [d]
           (+ a b c d)))))
   )
  1 2 3 4)


; ----------------#148
(= 233168 (__ 1000 3 5))
((fn [m a b]
   (let [ab (*' a b)
         _m (dec' m)
         na (quot _m a)
         nb (quot _m b)
         nab (quot _m ab)
         sum (fn [m n]
               (/ (*' (+' n (*' m n)) m) 2))]
     (println (sum na a) (sum nb b) (sum nab ab))
     (-' (+' (sum na a) (sum nb b)) (sum nab ab))
     ))
  (* 10000 10000 10000) 7 11)


; ----------------#171
(= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
   [[1 4] [6 6] [9 11] [13 17] [19 19]])

((fn [coll]
   (if (empty? coll)
     []
     (let [v (distinct (sort coll))]
       (loop [cur (list (first v))
              tar (rest v)
              ret '()]
         (println tar ret)
         (if (empty? tar)
           (for [r (reverse (cons (reverse cur) ret))]
             [(first r) (last r)])
           (if (= (inc (first cur)) (first tar))
             (recur (cons (first tar) cur) (rest tar) ret)
             (recur (list (first tar)) (rest tar) (cons (reverse cur) ret))
             )
           )
         ))))
  [])

; ----------------#177
(not (__ "[ { ] } "))
((fn [s]
   (loop [tar (map str (seq s))
          stack '()]
     (if (empty? tar)
       (empty? stack)
       (cond
         (re-find #"[\(\{\[]" (first tar)) (recur (rest tar) (cons (first tar) stack))
         (re-find #"[\)\}\]]" (first tar)) (if (some #(= % (str (first stack) (first tar))) ["()" "[]" "{}"])
                                             (recur (rest tar) (rest stack))
                                             false)
         :else (recur (rest tar) stack)))
     ))
  "class Test {
      public static void main(String[] args) {
        System.out.println(\"Hello world.\");
      }
    }")

; ----------------#150
(take 1
      ((fn pal [s]
         (let [rev (fn [k n]
                     (let [sk (seq (str k))]
                       (bigint
                         (apply str
                                (concat sk (if (odd? n)
                                             (rest (reverse sk))
                                             (reverse sk)))))))
               en (fn [n]
                    (bigint (apply str \1 (for [i (range n)] \0))))
               st (fn [n])
               f (fn [n]
                   (if (= n 1)
                     (range 10)
                     (let [i (quot (dec n) 2)]
                       ;(for [k (range (Math/pow 10 i) (Math/pow 10 (inc i)))]
                       ;  (rev (long k) n))
                       (map #(rev % n) (range (en i) (en (inc i))))
                       )))
               sn (count (str s))]
           ;(lazy-cat (drop-while #(< % s) (f sn)) (pal (en sn)))
           (f sn)
           ))
           ;(lazy-cat (drop-while #(< % s) (f sn)) (f (inc sn)))))
           ;(drop-while #(< % s) (f sn))
        (* 111111111 111111111))
      )

; drop whileが長過ぎるのでこいつでタイムアウトしている
; range en i からの開始を、指定数より起き鋳物にしてdrop-whileをなくす
(count (str (* 111111111 111111111)))


(take 12 ((fn f [a]
    (lazy-cat (range 0 9) (f 1))) 1))
