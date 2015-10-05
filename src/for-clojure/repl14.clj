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
(set (map #(first
;(take 26
      ((fn pal [s]
         (let [sn (count (str s))

               rev (fn [k n]
                     (let [sk (seq (str k))]
                       (bigint
                         (apply str
                                (concat sk (if (odd? n)
                                             (rest (reverse sk))
                                             (reverse sk)))))))

               en (fn [n]
                    (bigint (apply str \1 (for [i (range n)] \0))))

               st (fn [s sn]
                    (let [r (apply str (take (quot (inc sn) 2) (seq (str s))))]
                      (if (> s (rev r (count (str s))))
                        (inc (bigint r))
                        (bigint r))))

               f (fn [n]
                   (if (= n 1)
                     (range 10)
                     (let [i (quot (dec n) 2)]
                       (map (fn [a] (rev a n)) (range (st s sn) (en (inc i))))
                       )))]
           (lazy-cat (f sn) (pal (en sn)))
           ))
;        0))
        %)) (range 0 10000)))

;f ... 0.01 ~ 0.02s
;rev ... 0.005 ~ 0.002