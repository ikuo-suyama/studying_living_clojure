(ns for-clojure.repl8)

; ----------------#107
(=  (__ 10 [1 2 [3 [4 5] 6] 7])
    '(1 2 (3 (4))))

((fn mnest
   ([n coll] (mnest n 0 coll))
   ([n sum coll]
    (loop [_s sum
           _r coll
           ret []]
      (cond
        (> _s n) (if (nil? (butlast ret)) [] (butlast ret))
        (empty? _r) ret
        :else
        (if (coll? (first _r))
               (conj ret (mnest n _s (first _r)))
               (recur (+ _s (first _r)) (rest _r) (conj ret (first _r))))))))
  0 [1 2 [3 [4 5] 6] 7])

; [excellent]
(fn x [n v]
  (let [[f s] v]
    (cond (nil? f) nil
          (and (integer? f) (> f n)) []
          true
          (if (integer? f)
            (into [f] (x (- n f) (rest v)))
            [(x n (first v))]))))



; ----------------#113
; 1. clojure ではInterfaceのみを実装できる。
; defprotocol ... Interface
; extend-type recordの継承。

; problem seqというメソッドがあるため、
; seqを実装しようとすると
; CompilerException java.lang.ClassFormatError: Duplicate method name&signature

(defprotocol MySeq
  (seq2 [this]))
(defrecord MyColl [name]
  clojure.lang.Seqable
  (seq [this] '(:a :b))
  Object
  (toString [_] (apply str name)))

; これならできる
(.toString (let [f "foo"]
             (reify clojure.lang.Seqable
               (seq [this] (seq f))
               Object
               (toString [this] "a"))))
; -> a

; これならできる
(seq ((fn [a]
    (reify
      clojure.lang.Seqable
      (seq [this] '(1 2))
      Object
      (toString [this] "a"))) [1 2 3]))

;reify always implements clojure.lang.IObj and transfers meta
;data of the form to the created object.

; これでもできる
; Use reify where you would once use proxy, unless you need to override base class methods.
(seq (proxy [clojure.lang.Seqable] []
   (seq [] (seq '(1 2)))))

; proxy -> method書き換え、reify -> 実装した実メソッドを持つ匿名クラスのオブジェクトを返却、かな？

(= "1, 2, 3" (str (__ 2 1 3)))
(= '(2 1 3) (seq (__ 2 1 3)))

(seq ((fn [& coll]
    (reify
      clojure.lang.Seqable
      (seq [this] (if (empty? coll) nil (distinct coll)))
      Object
      (toString [this] (clojure.string/join ", " (sort coll)))))
   ))

; [excellent]
(fn [& x]
  (reify clojure.lang.ISeq
    (toString [_] (apply str (interpose ", " (sort x))))
    (seq [_] (seq (distinct x)))))



; ----------------#114
(= [2 3 5 7 11 13]
   (__ 4 #(= 2 (mod % 3))
       [2 3 5 7 11 13 17 19 23]))

((fn my-tw [n p coll]
   (loop [_c coll
          ret []
          count 0]
     (if (= count n)
       (butlast ret)
       (recur (rest _c) (conj ret (first _c)) (if (p (first _c)) (inc count) count))
       )))
  4 #(= 2 (mod % 3))
  [2 3 5 7 11 13 17 19 23])

; [excellent]
(fn gtw [i p s]
  (let [t take-while
        d drop-while
        c (comp not p)]
    (if (= i 1) (t c s)
                (concat (t c s)
                        [(first (d c s))]
                        (gtw (dec i) p (rest (d c s)))))))


; ----------------#115
;A balanced number is one whose component digits have the same sum
((fn balanced? [n]
   (loop [coll (map #(Integer/parseInt (str %)) (seq (str n)))
          suml 0
          sumr 0]
     (println coll suml sumr)
     (if (>= 1 (count coll))
       (= suml sumr)
       (recur (butlast (rest coll))
              (+ suml (first coll))
              (+ sumr (last coll))))))
  122)

; [excellent]
(fn b [n]
  (let [v (map #(read-string (str %)) (str n))
        m (quot (count v) 2)]
    (= (apply + (take m v)) (apply + (take-last m v)))))


; ----------------#116
(take 15 (filter
          (fn bp? [n]
            (letfn [(isprim? [n]
                             (cond (= 1 n) false
                                   (= 2 n) true
                                   (even? n) false
                                   :else ((fn prim? [_n coll]
                                            (cond (empty? coll) true
                                                  (zero? (mod _n (first coll))) false
                                                  :else (recur _n (rest coll))))
                                           n (filter odd? (range 3 (quot n 2))))))

                    (search [f n]
                            (let [_n (f n)]
                              (if (isprim? _n)
                                _n
                                (recur f _n))))

                    (beforep [n] (search #(- % 2) n))
                    (nextp [n] (search #(+ % 2) n))]

              (if (isprim? n)
                (if (= 2 n)
                  false
                  (= n (/ (+ (beforep n) (nextp n)) 2)))
                false)))
           (range)))

; [excellent]
(fn __ [n]
  (letfn [(sieve [s]
                 (cons (first s)
                       (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                                (rest s))))))]
    (let [primes (sieve (iterate inc 2))
          p (take-while #(<= % n) primes)]
      (and (= n (last p))
           (= n (/ (+ (first (take-last 2 p))
                      (last (take (inc (count p)) primes)))
                   2))))))


; ----------------#116
(= false (__ ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"]))

(= true  (__ ["#######"
              "#     #"
              "#  #  #"
              "#M # C#"
              "#######"]))

;↑(i-1, j)
;↓(i+1, j)
;←(i,   j+1)
;↓(i,   j-1)

((fn reachable? [_maze]
   (let [maze (vec (map vec _maze))
         MAXI (count maze)
         MAXJ (count (first maze))

         start (fn []
                 (filter (complement nil?)
                         (for [i (range MAXI)
                               j (range MAXJ)]
                           (if (= \M ((maze i) j)) [i j] nil))))

         nexts (fn [[ci cj]]
                 (set (for [[ni nj] [[(dec ci) cj]
                                     [(inc ci) cj]
                                     [ci (dec cj)]
                                     [ci (inc cj)]]
                            :when (and (<= 0 ni) (> MAXI ni)
                                       (<= 0 nj) (> MAXJ nj)
                                       (not (= \# ((maze ni) nj)))
                                       )]
                        [ni nj]
                        )))
         ]
     (letfn [(solv [nextpathes reached]
                   (println "[solv] nextpath:" nextpathes "reached:" reached)
                   (loop [current nextpathes
                          ret reached]
                     (println "[loop] current:" current "ret:" ret)
                     (if (empty? current)
                       ret
                       (let [_ns (nexts (first current))
                             ns (clojure.set/difference _ns ret)]
                         (println "let-ns:" _ns)
                         (if (not-empty ns)
                           (recur (rest current) (solv ns (clojure.set/union ret ns)))
                           (recur (rest current) ret)
                           )))))
             (reached? [coll]
                       ((complement nil?) (some #(= \C (let [[i j] %] ((maze i) j))) coll)))]

       (reached? (solv (start) #{}))
       )))
  ["M   C"])


; ----------------#118
(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))

(take 4
      ((fn my-map
         ([f coll] (my-map f (rest coll) (f (first coll))))
         ([f coll & ret]
          (if (not-empty coll)
            (lazy-seq (cons ret (my-map f (rest coll) (f (first coll))))))))
        inc (range)))

((fn my-map
   ([f coll] (my-map f (rest coll) (f (first coll))))
   ([f coll ret]
    (if (not-empty coll)
      (lazy-seq (cons ret (my-map f (rest coll) (f (first coll)))))
      (lazy-seq [ret]))))
  inc [2 3 4 5 6])

; [excellent]
(fn mp [f s]
  (when (seq s)
    (cons (f (first s)) (lazy-seq (mp f (rest s))))))


; ----------------#118
(= (__ :x [[:o :e :e]
           [:o :x :o]
           [:x :x :e]])
   #{[2 2] [0 1] [0 2]})

((fn ttt [p board]
   (let [MAXL (count board)
         MAXC (count (first board))
         CHK (concat
               (for [i (range MAXL)]
                 (for [j (range MAXC)]
                   [i j]))
               (for [i (range MAXL)]
                 (for [j (range MAXC)]
                   [j i]))
               ; cross
               '(([0 0] [1 1] [2 2])) '(([2 0] [1 1] [0 2])))

         e-inxs (fn []
                  (for [i (range MAXL)
                        j (range MAXC)
                        :when (= :e ((board i) j))]
                    [i j]))

         n-board (fn [coor]
                   (vec (for [i (range MAXL)]
                      (vec (for [j (range MAXC)]
                             (if (and (= i (first coor)) (= j (second coor)))
                               p
                               ((board i) j)))))))

         win? (fn [b l]
                (println b l)
                (every? #(= p ((b (first %)) (second %))) l))]

     (reduce
       (fn [ret e-coor]
         (println ret e-coor)
         (if (some #(win? (n-board e-coor) %) CHK)
           (conj ret e-coor)
           ret))
       #{}
       (e-inxs))
     )) :x
  [[:o :e :e]
   [:o :x :o]
   [:x :x :e]])

; [excellent]
(fn tic-tac [p b]
  (let [r [0 1 2]]
    (set (for [x r
               y r
               :when (and (= :e ((b x) y))
                          (let [nb (assoc-in b [x y] p)
                                w? (fn [s] (= [p] (distinct s)))]
                            (or (w? (nb x))
                                (w? (map #(% y) nb))
                                (and (= #{0 2} (into #{0 2} [x y]))
                                     (or (w? (map #((nb %) %) r))
                                         (w? (map #((nb %) (- 2 %)) r)))))))]
           [x y]))))


; ----------------#120
(= 8 (__ (range 10)))

((fn [coll]
   (letfn [(->seq [n]
                  (map #(Integer/parseInt (str %)) (vec (str n))))
           (calc [nseq]
                 (reduce #(+ % (* %2 %2)) 0 nseq))]

     (count (filter identity
              (map #(->>
                     (->seq %)
                     (calc)
                     (< %))
                   coll)))
     )) (range 10))