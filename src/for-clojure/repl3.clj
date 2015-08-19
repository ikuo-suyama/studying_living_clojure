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


; ----------------#58
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
; 実行直後（関数作成時）に関数が評価されると無限ループになってしまう。
; →関数内で関数を定義して、lazy-seqを返すように...
(take 10
      ((fn [func firstval coll]
         (letfn [(_reduct [_func _firstval _coll]
                          (lazy-seq (when-not (empty? _coll)
                                      (let [init (_func _firstval (first _coll))]
                                        (cons init (_reduct _func init (rest _coll)))))))]
           (lazy-seq (cons firstval (_reduct func firstval coll)))))
        + 0 (range)))

