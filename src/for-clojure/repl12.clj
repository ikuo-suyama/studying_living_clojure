(ns for-clojure.repl12)

(= (__ 2 4) [" 2 "
             "* 4"
             " * "])

(= (__ 3 81) [" 3 "
              "1 9"
              " 8 "])


(= (__ 4 20) [" 4 "
              "* 1"
              " 6 "])

(= (__ 2 256) ["  6  "
               " 5 * "
               "2 2 *"
               " 6 4 "
               "  1  "])

(= (__ 10 10000) ["   0   "
                  "  1 0  "
                  " 0 1 0 "
                  "* 0 0 0"
                  " * 1 * "
                  "  * *  "
                  "   *   "])

"1"

" 1 " <- "1 2"
"4 2" <- "   "
" 3 " <- "4 3"
;N=2 Size:4 Row:3
" 1 4 2 3 "


"  7  " <- "7 8 9"
" 6 8 " <- "     "
"5 1 9" <- "6 1 2"
" 4 2 " <- "     "
"  3  " <- "5 4 3"
;N=3 Size:9 Row:5
"  7   6 8 5 1 9 4 2   3  "
"  7   8 9 6 1 2 5 4   3  "


"   7   " <- "7 8 9 0"
"  6 8  " <- "       "
" 5 1 9 " <- "6 1 2 1"
"6 4 2 0" <- "       "
" 5 3 1 " <- "5 4 3 2"
"  4 2  " <- "       "
"   3   " <- "6 5 4 3"
;N=4 Size:16 Row:7

12
43

 1
4 2
 3
;N=2

789
612
543

i=3 2,1 1,2
i=4 2,2

7
68
519
42
3
;N=3


7890
6121
5432
6543
;N=4
((6)
  (5 7)
  (4 0 8)
  (15 3 1 9)
  (14 2 10)
  (13 11)
  (12))

3,1 2,2, 1,3
3,2 2,3
3,3

((fn [a b]
   (let [sq #(long (Math/pow % 2))
         ; display
         even-ss (fn [coll]
                  (let [n (count coll)
                        new (range (sq n) (sq (inc n)))]
                    (concat
                      (for [i (range n)]
                        (concat (nth coll i) (list (nth new i))))
                      (list (reverse (drop n new))))))

         odd-ss (fn [coll]
                   (let [n (count coll)
                         new (range (sq n) (sq (inc n)))]
                     (concat
                       (list (drop n new))
                       (for [i (range n)]
                         (concat (list (nth new (- n (inc i)))) (nth coll i))))))

         ss (fn _ss [n]
              (cond
                (= n 1) '((0))
                (odd? n) (odd-ss (_ss (dec n)))
                (even? n) (even-ss (_ss (dec n)))
                :else nil))

         rotate-45 (fn [coll]
                     (let [n (count coll)]
                       (concat
                         ; upper part
                         (for [i (range n)]
                           (for [j (range (inc i))]
                             (nth (nth coll (- i j)) j)))
                         ; lower part
                         (for [i (range 1 n)]
                           (for [j (range i n)
                                 :let [_i (+ i (- (dec n) j))]]
                             (nth (nth coll _i) j))))))

         fetch-seq (fn [coll indexes]
                     (for [lines indexes]
                       (for [i lines]
                         (coll i))))

         expand (fn [n coll]
                  (let [size (- (* 2 n) 1)]
                    (for [line coll]
                      (let [l (clojure.string/join " " line)
                            pad (apply str (take (quot (- size (count l)) 2) (cycle " ")))]
                        (str pad l pad))
                      )))

         ; calc
         s-seq (fn [a b]
                 (loop [ret (vector a)
                        nx  (sq a)]
                   (if (>= b nx)
                     (recur (conj ret nx) (sq nx))
                     ret)))

         pad-* (fn [v]
                 (let [se  (seq (apply str v))
                       cnt (count se)
                       len (first (drop-while #(< % cnt) (map sq (range))))]
                   (vec (take len (concat se (repeat \*))))))

         target (pad-* (s-seq a b))
         size (int (Math/sqrt (count target)))]

     (->> (ss size)
          (rotate-45)
          (fetch-seq target)
          (expand size))))
  2 4)

