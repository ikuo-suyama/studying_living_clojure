(ns for-clojure.repl7)

; ----------------#102
((fn ->camel [s]
   (if (< 0 (.indexOf s "-"))
     (let [_s (clojure.string/join (map clojure.string/capitalize (clojure.string/split s #"-")))]
       (apply str (clojure.string/lower-case (first _s)) (rest _s)))
     s))
  "leaveMeAlone")

; [excellent]
(fn t [s]
  (clojure.string/replace s #"\-[a-z]"  #(.toUpperCase (str (last %)))))


; ----------------#103
((fn [n coll]
   (set (filter #(= n (count %)) (reduce #(apply conj %1
                                             (for [v %1]
                                               (conj v %2))) #{#{}} coll))))
  1 #{4 5 6})


; ----------------#104
((fn ->roman [n]
   (let [TABLE {
                0 {1 "I"
                   2 "II"
                   3 "III"
                   4 "IV"
                   5 "V"
                   6 "VI"
                   7 "VII"
                   8 "VIII"
                   9 "IX"}
                1 {1 "X"
                   2 "XX"
                   3 "XXX"
                   4 "XL"
                   5 "L"
                   6 "LX"
                   7 "LXX"
                   8 "LXXX"
                   9 "XC"}
                2 {1 "C"
                   2 "CC"
                   3 "CCC"
                   4 "CD"
                   5 "D"
                   6 "DC"
                   7 "DCC"
                   8 "DCCC"
                   9 "CM"}
                3 {1 "M"
                   2 "MM"
                   3 "MMM"}}
         nums (vec (reverse (str n)))
         ->int #(- (int %) (int \0))]
     (apply str (reverse (for [i (range (count nums))]
                 ((TABLE i) (->int (nums i)))
                 ))))) 3999)

; [excellent]
; 見栄えがいいだけだが
(fn [x] (let [n [(zipmap (range 0 10) '("" "I" "II" "III" "IV" "IV" "VI" "VII" "VIII" "IX"))
                 (zipmap (range 0 10) '("" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"))
                 (zipmap (range 0 10) '("" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"))
                 (zipmap (range 0 4) '("" "M" "MM" "MMM"))
                 ]] (apply str (reverse (map-indexed #((n %) %2) (reverse (map #(- (int %) 48) (str x))))))))


; ----------------#105
(= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))

((fn [coll]
   (let [_coll (reduce #(if (and (keyword? (last %1)) (keyword? %2))
                         (conj %1 nil %2)
                         (conj %1 %2)) [] coll)]
     (reduce #(conj % [(first (key %2)) (vec (filter (complement nil?) (val %2)))]) {}
             (apply hash-map (partition-by keyword? _coll)))))
  [:a 1 2 3 :b :c 4])


; ----------------#106
((fn maze [a b]
   (letfn [(calc [coll]
                 (let [c (last coll)
                       d (conj coll (* c 2))
                       p (conj coll (+ c 2))
                       h (conj coll (/ c 2))]
                   (distinct (filter #(and (apply distinct? %) (integer? (last %))) [p d h]))
                   ))

           (next-path [coll]
                      (reduce #(if (empty? %2) % (apply conj % %2)) []
                              (for [x coll] (calc x))))
           ]

     (loop [hists [[a]]]
       (println hists)
       (if (some #(= b (last %)) hists)
         (count (first hists))
         (recur (next-path hists))
         )))) 9 2)

; [excellent]
(fn f [x y & [c]]
  (let [m 10
        c (or c 0)
        d (+ 1 c)]
    (cond (= x y) 1
          (> c m) m
          true (inc (min (f (+ 2 x) y d)
                         (f (* 2 x) y d)
                         (if (odd? x) m (f (/ x 2) y d)))))))


; ----------------#107
(= 256 (((fn [n]
           (fn [x] (long (Math/pow x n)))) 2) 16),
   (((fn [n]
       (fn [x] (long (Math/pow x n)))) 8) 2))

(fn [n]
  (fn [x] (long (Math/pow x n))))

; ----------------#107
(= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))

((fn cm
   ([a] (first a))
   ([a & b]
    (let [test (first a)
          seek (fn [coll n]
                 (loop [_c coll]
                   (if (<= n (first _c))
                     _c
                     (recur (rest _c)))))
          ret (for [target b]
                (seek target test))]
      (if (every? #(= test (first %)) ret)
        test
        (apply cm (rest a) ret)))))
  (range) (range 0 100 7/6) [2 3 5 7 11 13])

; [excellent]
(fn f [& s]
  (if (= 1 (count (distinct (map first s)))) (first (first s))
                                             (let [m (apply max (map first s))
                                                   new-s (map #(if (< (first %) m) (rest %) %) s)]
                                               (apply f new-s))))


; ----------------#110
(take 7 (
          (fn lzpn [coll]
            (let [pronunciation (fn [_coll]
                                  (reduce #(conj % (count %2) (first %2)) []
                                          (partition-by identity _coll)))
                  p (pronunciation coll)]
              (lazy-seq (cons p (lzpn p)))))
          [1]))

; [excellent]
; Returns a lazy sequence of x, (f x), (f (f x)) etc.
#(rest (iterate
         (fn [s] (mapcat
                   (fn [x] (cons (count x) [(first x)])) (partition-by identity s)))
         %))

; ----------------#111
(= false (__ "the" ["c _ _ _"
                    "d _ # e"
                    "r y _ _"]))

(= true  (__ "joy" ["c _ _ _"
                    "d _ # e"
                    "r y _ _"]))

(= true  (__ "clojure" ["_ _ _ # j o y"
                        "_ _ o _ _ _ _"
                        "_ _ f _ # _ _"]))

((fn [s board]
   (let [readh (fn [board]
                 (for [l board]
                   (.replace l " " "")))

         readv (fn [board]
                 (map #(apply str %) (filter (fn [c] (not (every? #(= " " %) c)))
                          (for [i (range (count (first board)))]
                            (for [j (range (count board))]
                              (subs (board j) i (inc i)))
                            ))))

         words (flatten
                 (map #(seq (.split % "#"))
                      (flatten (conj (readh board) (readv board)))))

         ->seq (fn [str]
                (set (map-indexed #(vector % %2) str)))

         defs (fn [a b]
                (clojure.set/difference (->seq b) (->seq a)))

         same? (fn [s t]
                (if (= (count s) (count t))
                  (every? #(= "_" (str (second %))) (defs s t))
                  false))]

     (loop [_t words]
       (println _t)
       (if (empty? _t)
         false
         (if (same? s (first _t))
           true
           (recur (rest _t)))))))
  "joy" ["c _ _ _"
         "d _ # e"
         "r y _ _"])
