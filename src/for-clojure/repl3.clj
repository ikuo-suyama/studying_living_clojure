(ns for-clojure.repl3)

; ----------------#51
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

; ----------------#52
(= [2 4] (let [[a b c d e f g] (range)] [c e]))


; ----------------#53
(= ((fn [coll]
      (let [x (last (sort-by count (reduce #(if (= (last (last %1)) (dec %2))
                                             (conj (pop %1) (conj (last %1) %2))
                                             (conj %1 (vector %2)))
                                           [] coll)))]
        (if (= 1 (count x)) [] x))) [1 0 1 2 3 0 4 5]) [0 1 2 3])

; [excellent]
(fn [v]
  (or
    (first (filter #(apply < %)
                   (mapcat #(partition % 1 v) (range (count v) 1 -1))))
    []))

; 全部の組み合わせを作って、apply で一つ前より大きい物だけ抜き出している！
((fn [v] (mapcat #(partition % 1 v)
                 (range (count v) 1 -1))) [1 0 1 2 3 0 4 5])
;=> ((1 0 1 2 3 0 4 5) (1 0 1 2 3 0 4) (0 1 2 3 0 4 5) (1 0 1 2 3 0) (0 1 2 3 0 4) (1 2 3 0 4 5) (1 0 1 2 3) (0 1 2 3 0) (1 2 3 0 4) (2 3 0 4 5) (1 0 1 2) (0 1 2 3) (1 2 3 0) (2 3 0 4) (3 0 4 5) (1 0 1) (0 1 2) (1 2 3) (2 3 0) (3 0 4) (0 4 5) (1 0) (0 1) (1 2) (2 3) (3 0) (0 4) (4 5))

(apply < '(1 2 3 0))
;=> false
(apply < '(0 1 2 3))
;=> true



; ----------------#54
(= ((fn [n coll]
      (filter #(= n (count %)) (partition-by #(quot % n) coll)))
     3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))

(fn [n coll]
  (filter #(= n (count %)) (partition-by #(quot % n) coll)))

; [excellent]
(fn p [n s]
  (if (> n (count s))
    []
    (cons (take n s) (p n (drop n s)))))



; ----------------#55
(= ((fn [coll]
      (into {}
            (map #(vector (first %) (count %))
                 (partition-by identity (sort coll))))) [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

((fn [coll]
   (into {}
         (map #(vector (first %) (count %))
              (partition-by identity (sort coll))))) [1 1 2 3 2 1 1])

; [excellent]
(fn [s] (reduce #(assoc % %2 (inc (% %2 0))) {} s))
; assoc override the old value



; ----------------#56
(= ((fn [coll]
      (reduce #(if (<= 0 (.indexOf %1 %2))
                %1
                (conj %1 %2)) [] coll)) [1 2 1 3 1 2 4]) [1 2 3 4])

(reduce #(if (<= 0 (.indexOf %1 %2))
          %1
          (conj %1 %2)) [] [:a :a :b :b :c :c])

; [excellent]
(fn [s] (remove nil? (map #(if (%2 %1) nil %1) s (reductions conj #{} s))))



; ----------------#57
(= [3 2 1] (((fn
               ([f g]
                (fn [& x] (f (apply g x))))
               ([f g h]
                (fn [& x] (f (g (apply h x)))))) rest reverse) [1 2 3 4]))

(fn
  ([f g]
   (fn [& x] (f (apply g x))))
  ([f g h]
   (fn [& x] (f (g (apply h x))))))

; [excellent]
(fn [& f]
  (fn [& a]
    (reduce #(%2 %) (apply (last f) a) (rest (reverse f)))))


; ----------------#58
(= [21 6 1] (((fn [& f]
                (fn [& x]
                  (reduce #(conj % (apply %2 x)) [] f))) + max min) 2 3 5 1 6 4))

(fn [& f]
  (fn [& x]
    (reduce #(conj % (apply %2 x)) [] f)))

; [excellent]
(fn juxt' [& fs]
  (fn [& xs]
    (mapv #(apply % xs) fs)))


; ----------------#60
;(= (take 5 (__ + (range))) [0 1 3 6 10])
;(0 1 2 3)

; lazy-seq sample
; (lazy-seq &body) 次に対象のSeqが評価される際、Bodyの内容を一回だけ評価する。
(take 5 ((fn double-coll [coll]
           (lazy-seq (cons (inc (first coll)) (double-coll (rest coll))))) (range)))

; defalut-value
((fn test
   ([a b] (test a b 3))
   ([a b c] (print c))) 1 2)
; or
;((fn test [a b & {:keys [c] :or {c 3}}]
;   (print c)) 1 2)


;(take 5
;      ((fn red
;         ([func coll] (red func coll [0]))
;         ([func coll let]
;           (lazy-seq (red func (rest coll) (conj let (func (first coll) (last let)))))))
;        +
;        (range)))

(let [r (rest (range 5))]
  (realized? r))
;-> error(not lazy seq)
;ClassCastException clojure.lang.ChunkedCons cannot be cast to clojure.lang.IPending  clojure.core/realized? (core.clj:6883)

(let [r (lazy-seq (rest (range 5)))]
  (realized? r))
;=> false
; lazy

(let [r (lazy-seq (cons 1 (rest (range 5))))]
  (realized? r))
;=> false
; lazy

(let [r (lazy-seq (cons 1 (rest (range))))]
  (realized? r))
;=> false
; still lazy

; 1.
(take 10 ((fn [coll]
            (lazy-seq (rest coll))) (range)))

; 2.
(take 10 ((fn [coll]
            (cons (first coll) (lazy-seq (rest coll)))) (range)))


; 3.
(take 10
      ((fn myself [first coll]
         (if-not (= 11 (first coll))
           (lazy-seq (myself (rest coll)))
           (cons first (lazy-seq (+ first (first coll)))))) (range)))

; http://www.thesoftwaresimpleton.com/blog/2014/09/08/lazy-seq/
; now trying...
; cons で後ろにつないでいく、値を返すために再帰呼び出しするところがミソ
(take 10
      ((fn myreduct
         ([func coll]
          (myreduct func (first coll) (rest coll)))
         ([func firstval coll]
          (letfn [(_reduct [_func _firstval _coll]
                           (lazy-seq (when-not (empty? _coll)
                                       (let [init (_func _firstval (first _coll))]
                                         (cons init (_reduct _func init (rest _coll)))))))]
            (lazy-seq (cons firstval (_reduct func firstval coll))))))
        + (range)))

; [excellent]
(fn r
  ([f c] (r f (first c) (rest c)))
  ([f i c]
   (if (empty? c)
     (cons i nil)
     (lazy-seq (cons i (r f (f i (first c)) (rest c)))))))


(take 5 ((fn myred2
           ([func coll] (myred2 func (first coll) (rest coll)))
           ([func val coll]
            (if (empty? coll)
              (cons val nil)
              (lazy-seq (cons val (myred2 func (func val (first coll)) (rest coll)))))))
          + (range)))

; 実行して展開されるとこんなイメージ？
(take 3 (lazy-seq (cons 0 (lazy-seq (cons 1 (lazy-seq (cons 2 (lazy-seq (cons 3 (cons 4 nil))))))))))


; ----------------#61
(= ((fn [coll1 coll2]
      (reduce merge {} (map hash-map coll1 coll2))) [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})

(fn [coll1 coll2]
  (reduce merge {} (map hash-map coll1 coll2)))

; [excellent]
; Returns a lazy seq of the first item in each coll, then the second etc.
#(apply assoc {} (interleave % %2))

(interleave [:a :b :c] [1 2 3])
; => (:a 1 :b 2 :c 3)



; ----------------#62
(= (take 5 ((fn myiterate [f v]
              (lazy-seq (cons v (myiterate f (f v))))) #(* 2 %) 1)) [1 2 4 8 16])

; [excellent]
(fn ! [f x] (cons x (lazy-seq (! f (f x)))))



; ----------------#63
(= ((fn mygroupby [test coll]
      (reduce #(assoc % (test %2) (conj (get % (test %2) []) %2)) {} coll)) #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})

; [excellent]
; Returns a map that consists of the rest of the maps conj-ed onto the first
(fn gb2 [f vals]
  (reduce #(merge-with into %1 {(f %2) [%2]}) {} vals))



; ----------------#64
(defn checkcoltype [target]
   (cond
     (= :val (:key (conj target [:key :val]))) :map
     (= (inc (count target)) (count (conj target 1 1))) :set
     (= 2 (first (conj target 1 2))) :list
     (= 2 (last (conj target 1 2))) :vector
     :else :unknown)
   )

; [excellent]
(fn __ [x]
  (cond (reversible? x) :vector
        (associative? x) :map
        (= (conj x 1 1) (conj x 1)) :set
        :else :list))

; [excellent2]
; not knocking!
#((zipmap (map str ['() [] {} #{}]) [:list :vector :map :set]) (str (empty %)))
; (empty {:key :val})
;=> {}
(comp {\{ :map \# :set \[ :vector \c :list} first str)


; ----------------#66
(= ((fn [a b]
      (let [x (min a b)
            y (max a b)]
        (letfn [(ld [v]
                    (filter #(zero? (mod v %)) (reverse (range 1 (inc v)))))]
          (first (filter #(and (zero? (mod x %)) (zero? (mod y %))) (ld x)))
          ))) 1023 858) 33)

; 1. 公約数の配列
(filter #(zero? (mod 20 %)) (reverse (range 1 21)))


; [excellent]
(fn g [a b]
  (if (= 0 a)
    b
    (g (mod b a) a)))


; ----------------#67
; primnumber

(fn [c]
  (take c (filter (fn isprim? [n]
                    (cond (= 1 n) false
                          (= 2 n) true
                          (even? n) false
                          :else ((fn prim? [_n coll]
                                   (cond (empty? coll) true
                                         (zero? (mod _n (first coll))) false
                                         :else (recur _n (rest coll))))
                                  n (filter odd? (range 3 (quot n 2))))))
                  (range))))

;is prim
(defn isprim? [n]
   (cond (= 1 n) false
         (= 2 n) true
         (even? n) false
         :else ((fn prim? [_n coll]
                  (cond (empty? coll) true
                        (zero? (mod _n (first coll))) false
                        :else (recur _n (rest coll))))
                 n (filter odd? (range 3 (quot n 2))))))

;is prim core
((fn prim? [_n coll]
   (cond (empty? coll) true
         (zero? (mod _n (first coll))) false
         :else (recur _n (rest coll))))
  10 (filter odd? (range 3 (quot 10 2))))

; 実行速度/BigNumberを気にしなければ..
(fn [n] (take n (filter #(empty? (for [i (range 2 (inc (quot % 2))) :when (zero? (mod % i))] i)) (range 2 999))))



; ----------------#68
(= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))
((fn [f init & x]
   (reduce
     (fn [a b]
       (reduce #(merge %1
                       (cond
                         (and (contains? a %2) (contains? b %2)) {%2 (f (get a %2) (get b %2))}
                         (contains? b %2) {%2 (get b %2)}))
               a (keys b)))
  init x))
  - {1 10, 2 20} {1 3, 2 10, 3 15})

; [excellent]
(fn [f & a]
  (into {}
        (map
          (fn [k] [k (reduce f (keep #(% k) a))])
          (keys (apply merge a)))))


; ----------------#68
(= (__  "Have a nice day.")
   ["a" "day" "Have" "nice"])

((fn [str]
   (sort-by #(-> % .toLowerCase)
            (-> str
                (.replaceAll "\\.|!" "")
                (.split " "))))
  "Have a nice day!")

; [excellent]
#(sort-by clojure.string/lower-case (re-seq #"\w+" %))
; re-seq
; Returns a lazy sequence of successive matches of pattern in string

