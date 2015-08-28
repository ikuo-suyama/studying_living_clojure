(ns for-clojure.repl6)

; [excellent]

; ----------------#91
(= false (__ #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]}))

((fn connected? [coll]
   (letfn [(nodes [_coll]
                  (distinct (flatten _coll)))

           (edge? [node tuple]
                  (contains? (set tuple) node))

           (next-node [node edge]
                      (if (apply distinct? edge)
                        (first (disj (set edge) node))
                        (first edge)))

           (_remove [coll x]
                    (let [n (.indexOf coll x)]
                      (cond
                        (= 0 n) (vec (rest coll))
                        (= (count coll) (inc n)) (vec (butlast coll))
                        :else (apply conj (subvec coll 0 n) (subvec coll (inc n))))))

           (walk [node edges done]
                  (if (empty? edges)
                    (conj [] edges (distinct done))
                    (loop [_current edges]
                      (println "debug!:" node edges _current done)
                      (if (empty? _current)
                        ; return value of path?
                        (conj [] edges (distinct done))
                        (let [e (first _current)]
                          (if (edge? node e)
                            (walk (next-node node e) (_remove edges e) (apply conj done e))
                            (recur (rest _current))))
                        ))))]

     (loop [start (first (nodes (vec coll)))
            edges coll]
       (let [ret (walk start (vec edges) [])
             yet-edge (first ret)
             done-node (last ret)
             startable (clojure.set/intersection (set (nodes yet-edge)) (set done-node))]

         (println yet-edge done-node startable)
         (if (empty? yet-edge)
           true
           (if (empty? startable)
             false
             (recur (first startable) yet-edge)))
         ))
     ))
  {[:a :b] [:b :c] [:c :d]
           [:x :y] [:d :a] [:b :e]})

; [excellent]
(fn __ [edges]
  (letfn [(connected? [c1 c2]
                      (not (empty? (clojure.set/intersection c1 c2))))
          (add-comp [c1 c2]
                    (if (connected? c1 c2) (apply conj c1 c2) c1))
          (merge-comp [cs]
                      (if (empty? cs) '()
                                      (let [c (first cs), rs (rest cs)]
                                        (if (true? (some true? (map #(connected? c %) rs)))
                                          (merge-comp (map #(add-comp % c) rs))
                                          (cons c (merge-comp rs))))))
          ]
    (->> (map #(into #{} %) edges)
         (merge-comp)
         (count)
         (#(= 1 %)))))



; ----------------#92
(= 3999 (__ "MMMCMXCIX"))


(
  (fn read-roman [str]
    (letfn [(big? [R a b]
                  (< (.indexOf R a) (.indexOf R b)))]

      (let [ROMAN "IVXLCDM"
            TABLE {"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}]
        (loop [roman (.split str "")
;        WORNING: 4clojure use java 7!!
;        (loop [roman (rest (.split str ""))
               ret 0]
          (if (second roman)
            (if (big? ROMAN (first roman) (second roman))
              (recur (rest roman) (- ret (get TABLE (first roman))))
              (recur (rest roman) (+ ret (get TABLE (first roman)))))
            (+ ret (get TABLE (first roman)))
            )))))
  "XIV")

; [excellent]
;#(->> (-> %2
;          (% #"IV" " 4")
;          (% #"IX" " 9")
;          (% #"XL" " 40")
;          (% #"XC" " 90")
;          (% #"CD" " 400")
;          (% #"CM" " 900")
;          (% #"I" " 1")
;          (% #"V" " 5")
;          (% #"X" " 10")
;          (% #"L" " 50")
;          (% #"C" " 100")
;          (% #"D" " 500")
;          (% #"M" " 1000")
;          clojure.string/trim
;          (clojure.string/split #"\s"))
;      (map read-string)
;      (apply +))
;clojure.string/replace

; ----------------#93
(fn [coll] (filter #(and (coll? %) (not (coll? (first %)))) (tree-seq coll? identity coll)))



; ----------------#94
(= (__
   ["      "
    " ##   "
    " ##   "
    "   ## "
    "   ## "
    "      "])
   ["      "
    " ##   "
    " #    "
    "    # "
    "   ## "
    "      "])

((fn life-game [coll]
   (letfn [(init [_coll]
                 (reduce #(conj %1 (vec %2)) [] coll))

           (neighbers [_coll i j lm cm]
                      (for [x [(dec i) i (inc i)]
                            y [(dec j) j (inc j)]
                            :when (and (<= 0 x) (<= 0 y)
                                       (<= x lm) (<= y cm)
                                       (not (and (= x i) (= y j))))]
                        ((_coll x) y)))]

     (let [LMAX (count coll)
           CMAX (count (first coll))
           _coll (init coll)]

       (reduce #(conj %1 (apply str %2)) []
         (partition CMAX
                   (for [i (range LMAX)
                         j (range CMAX)
                         :let [self ((_coll i) j)]]
                     (let [nlist (group-by str (neighbers _coll i j (dec LMAX) (dec CMAX)))
                           lives (count (nlist "#"))
                           dies (count (nlist " "))]
                       (if (= (str self) "#")
                         ;live cell
                         (if (or
                               ; rule 1
                               (> 2 lives)
                               ; rule 2
                               (< 3 lives))
                           " "
                           "#")
                         (if (= 3 lives)
                           "#"
                           " "))
                       )))))))
  ["      "
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "])



