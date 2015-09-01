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



; ----------------#95
(= (__ '(:a (:b nil nil) nil))
   true)

(= (__ '(:a (:b nil nil)))
   false)

((fn is-bt? [coll]
   (if (and (coll? coll) (= 3 (count coll)))
     (let [l (second coll)
           r (last coll)]
       (and (if (nil? l) true (is-bt? l))
            (if (nil? r) true (is-bt? r))))
     false)) [1 [2 [3 [4 false nil] nil] nil] nil])


; ----------------#96
(= ((fn is-symt? [coll]
      (letfn [(rev-tree [coll]
                        (let [v (first coll)
                              l (second coll)
                              r (last coll)]
                          (conj [] v
                                (if (coll? r) (rev-tree r) r)
                                (if (coll? l) (rev-tree l) l))))]
        (= (second coll) (rev-tree (last coll)))))

     [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(fn is-symt? [coll]
  (letfn [(rev-tree [coll]
              (let [v (first coll)
                    l (second coll)
                    r (last coll)]
                (conj [] v
                      (if (coll? r) (rev-tree r) r)
                      (if (coll? l) (rev-tree l) l))))]
    (= (second coll) (rev-tree (last coll)))))

; [excellent]
#(= % ((fn mir [[r le ri :as tr]]
         (if tr (conj [] r (mir ri) (mir le)))) %))


; ----------------#97
(= (map __ (range 1 6))
   [     [1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])

((fn [n]
   (loop [i 1
          ret [0 1 0]]
     (if (= i n)
       (filter #(not (zero? %)) ret)
       (recur (inc i)
              (conj (apply conj [] 0 (vec (for [x (range (inc i))]
                                       (+ (ret x) (ret (inc x)))))) 0)
              ))))
  4)

; [excellent]
(fn pt [n]
  (if
    (= 1 n) [1]
            (concat [1] (map + (pt (dec n)) (rest (pt (dec n)))) [1])))

; ----------------#98
; かんちがい
;((fn np2 [coll]
;   (loop [_c (vec coll)
;          ret []]
;     (println _c ret)
;     (if (next _c)
;       (recur (vec (rest _c))
;              (apply conj ret (for [i (range (dec (count _c)))]
;                 [(_c i) (_c (inc i))])))
;       ret))) #{1 2 4})

(= ((fn [f coll]
      (reduce #(conj %1 (set (val %2))) #{} (group-by f coll))) #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})

(fn [f coll]
  (reduce #(conj %1 (set (val %2))) #{} (group-by f coll)))

; [excellent]
#(set (map set (vals (group-by % %2))))


; ----------------#99
((fn [x y]
   (reduce #(conj % (. Long parseLong %2)) [] (rest (clojure.string/split (str (* x y)) #"")))) 9 9)

; [excellent]
(comp #(map read-string (map str %)) str *)


; ----------------#99
((fn lcm [& args]
   (let [m (apply max args)]
     (loop [n 1]
       (if (every? #(zero? (mod (* m n) %)) args)
         (* m n)
         (recur (inc n)))))) 3/4 1/6)


; ----------------#101
((fn lev-dis [a b]
   (letfn [(->seq [str]
                  (set (map-indexed #(vector % %2) str)))

           (distance [a b]
                     (count (clojure.set/difference (->seq a) (->seq b))))

           (closest [a b]
                    (first (sort-by #(distance a %) <
                                    (for [i (range 0 (count b))]
                                      (str (subs b 0 i) (subs b (inc i)))))))
           (to-string [s]
                      ; just cheating about keyword
                      (.replace (apply str s) ":" ""))]

     (let [counta (count a)
           countb (count b)]
       (cond
         (not (string? a)) (lev-dis (to-string a) (to-string b))
         (> counta countb) (lev-dis b a)
         (= counta countb) (distance a b)
         (< counta countb)
         (loop [c b
                n 0]
           (let [cl (closest a c)]
             (if (= counta (count cl))
               (+ (inc n) (distance a cl))
               (recur cl (inc n))
               )))))))
  '(:a :b :c :d) '(:a :d))

; [excellent]
; 1文字ずつ削って、lastあってればcost 0, あってなければ１を足し上げる
; そのminを取る
((fn [s t]
   (let [f (fn [f s t]
             (let [sc (count s)
                   tc (count t)]
               (println s t)
               (cond
                 (= sc 0) tc
                 (= tc 0) sc
                 :else (let [cost (if (= (last s) (last t)) 0 1)]
                         (min (inc (f f (butlast s) t))
                              (inc (f f s (butlast t)))
                              (+ (f f (butlast s) (butlast t)) cost))))))
         mf (memoize f)]
     (mf mf s t)))
  "kitten" "sitting")