(ns for-clojure.repl2)

; ----------------#31
(= ((fn [x]
      (reverse (reduce #(if (= (first (first %1)) %2)
                         (conj (rest %1) (conj (first %1) %2))
                         (conj %1 (list %2)))
                       () x)))
     [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))

; [excellent]
; partition-by identity

; Applies f to each value in coll, splitting it each time f returns a new value.
;                                                                     ^^^^^^^^^^


; ----------------#32
(= (mapcat #(vector % %) [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

; [excellent]
#(interleave % %)
;interleave ... Returns a lazy seq of the first item in each coll, then the second etc.

(interleave [:a :b :c] [1 2 3])
;;=> (:a 1 :b 2 :c 3)


; ----------------#33
(= ((fn [coll n]
      (mapcat #(take n (cycle (list %))) coll)) [:a :b] 4) '(:a :a :a :a :b :b :b :b))

; [excellent]
#(mapcat (partial repeat %2) %)

; partitial
; Takes a function f and fewer than the normal arguments to f, and
; returns a fn that takes a variable number of additional args

;(partial repeat %2)
;=
;(fn [x]
;  (repeat %2 x))


; ----------------#34
(= (#(reverse
      (loop [coll ()
             start %1
             end %2]
        (if (< start end)
          (recur (conj coll start) (inc start) end)
          coll))) 1 4) '(1 2 3))

; [excellent]
#(take (- %2 %) (iterate inc %))
; iterate ... create infinite lazy seq by f


; ----------------#37
; regular Expressions
(re-seq #"\d" "clojure 1.1.0")
;=> ("1" "1" "0")


; ----------------#38
(= ((fn [& args]
      (reduce (fn [a b] (if (> a b) a b)) args))
     1 8 3 4) 8)

; [excellent]
#(- (apply min (map - %&)))
; 全部マイナスにして、MINして再度マイナス！


; ----------------#39
(= (mapcat #(vector %1 %2) [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))

; [excellent]
; (mapcat vector)

; ----------------#40
(= ((fn [x coll]
      (rest (mapcat #(conj (list %1) x) coll))) 0 [1 2 3]) [1 0 2 0 3])


; [excellent]
#(rest (mapcat list (repeat %1) %2))


; ----------------#41
(= ((fn [coll n]
      (map
        #(last %)
        ; create ([inx num] ...)
        (filter
          #((complement zero?) (rem (+ 1 (first %)) n))
          (map-indexed vector coll))))
     [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])


; [excellent]
(fn [a x]
  (keep-indexed #(if (not= 0 (mod (inc %) x)) %2) a))
; keep-indexed f coll -> fn [idx v] create only return value
; when f returns nil the index is not included in final result


; ----------------#42
;1 1
;3 6
;5 120
;8 40320
;
;2 * 3
;2 * 3 * 4 * 5
(= (#(reduce * (range 1 (+ 1 %))) 8) 40320)

; [excellent]
;#(apply * (range 1 (inc %)))


; ----------------#43
(= ((fn [coll n]
      (partition-by #(rem % n) (sort-by #(rem % n) coll)))
     (range 10) 5)
   '((0 5) (1 6) (2 7) (3 8) (4 9)))

(= ((fn [coll n]
      (partition-by #(rem % n) (sort-by #(rem % n) coll))) [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
; No 1. NG

(fn [coll n]
  (map #(second %) (group-by #(rem % n) coll)))
(= ((fn [coll n]
      (map #(second %) (group-by #(rem % n) coll))) [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))

; [excellent]
#(apply map list (partition %2 %1))

;apply
(max [1 2 3])
;;=> [1 2 3]

(apply max [1 2 3])
;;=> 3

;; which is the same as
(max 1 2 3)
;;=> 3

; ----------------#44
(= (#(flatten
      (reverse
        (split-at
          (let [go (rem %1 (count %2))]
            (if (neg? go) (+ go (count %2)) go))
          %2)))
     2 [1 2 3 4 5]) '(3 4 5 1 2))

#(flatten
  (reverse
    (split-at
      (let [go (rem %1 (count %2))]
        (if (neg? go) (+ go (count %2)) go))
      %2)))

; [excellent]
#(let [n (mod %1 (count %2))]
  (concat (drop n %2) (take n %2)))
;; The mod function is defined as the amount by which a number exceeds the
;; largest integer multiple of the divisor that is not greater than that number.
;; The largest integer multiple of 5 not greater than -2 is 5 * -1 = -5.
;; The amount by which -2 exceeds -5 is 3.


; ----------------#45
(take 5 (iterate inc 5))
;(5 6 7 8 9)


; ----------------#46
(= 3 (((fn [func] #(func %2 %1)) nth) 2 [1 2 3 4 5]))

(fn [func] #(func %2 %1))


; ----------------#47
; to Set .. valule
(contains? #{4 5 6} 4)
; to Vec ... index
(contains? [1 1 1 1 1] 4)
; to Map ... key
(contains? {4 :a 2 :b} 4)

; ----------------#48
(= 6 (some #{2 7 6} [5 6 7 8]))
; return first true value


; ----------------#49
(= (#(vector (take %1 %2) (drop %1 %2)) 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
; [excellent]
#(list (take % %2) (drop % %2))


; ----------------#50
(= (set ((fn [x] (map #(second %) (group-by #(class %) x))) [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})

; [excellent]
#(vals (group-by class %))
