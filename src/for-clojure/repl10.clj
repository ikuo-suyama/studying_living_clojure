(ns for-clojure.repl10)

; ----------------#127
(= 10 (__ [15 15 15 15 15]))
; 1111      1111
; 1111      *111
; 1111  ->  **11
; 1111      ***1
; 1111      ****

(= 15 (__ [1 3 7 15 31]))
; 00001      0000*
; 00011      000**
; 00111  ->  00***
; 01111      0****
; 11111      *****

(= 3 (__ [3 3]))
; 11      *1
; 11  ->  **

(= 6 (__ [17 22 6 14 22]))
; 10001      10001
; 10110      101*0
; 00110  ->  00**0
; 01110      0***0
; 10110      10110

; 特別タイプとして扱う？
(= 9 (__ [18 7 14 14 6 3]))
; 10010      10010
; 00111      001*0
; 01110      01**0
; 01110  ->  0***0
; 00110      00**0
; 00011      000*1

(= 4 (__ [7 3]))
; 111      ***
; 011  ->  0*1

; ※正方形に補正

((fn triangle [coll]
   (let [rocks (let [_rb (for [x coll]
                          (Integer/toBinaryString x))
                     ;正方形桁埋めあり
;                     _maxc (apply max (conj (map count _rb) (count _rb)))
                     _maxc (apply max (map count _rb))
                     _maxl (count _rb)
                     _v (vec (map #(vec (format (str "%0" _maxc "d")
                                            (Integer/parseInt %)))
                              _rb))]
                 (for [x (range _maxl)]
                   (for [y (range _maxc)]
                     (if (< x (count _v))
                       (Integer/parseInt (str ((_v x) y)))
                       0))))
         LOWX (count rocks)
         COLY (count (first rocks))

         ; utils
         debug (fn _d
                 ([target] (_d "debug" target))
                 ([name target]
                 (println name "::---")
                 (loop [_c target]
                   (println (apply str (first _c)))
                   (if (not-empty _c)
                     (recur (rest _c))
                     target))))

         debug-b (fn [b]
                   (debug "---- shifted"
                          (partition-all COLY
                                         (seq (.replace
                                                (format (str "%" (* LOWX COLY) "s")
                                                        (Integer/toBinaryString b))
                                                " " "0"))))
                   b)

           to-b (fn [coll]
                (Long/parseLong (apply str (flatten coll)) 2))
         countm (fn [mine]
                  (reduce + (filter #(= 1 %) (flatten mine))))
         padding-y (fn [mask]
                     (let [n (- COLY (count (first mask)))]
                       (if (> n 0)
                         (for [l mask]
                           (concat l (take n (cycle '(0)))))
                         mask)))
         padding-x (fn [mask]
                     (let [n (- LOWX (count mask))]
                       (if (> n 0)
                         (partition-all COLY
                           (concat (flatten mask)
                                  (take (* n COLY) (cycle '(0)))))
                         mask)))
         padding (fn [mask]
                   (-> mask
                       (padding-y)
                       (padding-x)))

         ; create - triangle
         create-e (fn [size]
                    ; for even
                    (for [x (range size)]
                      (for [y (range size)]
                        (if (>= x y) 1 0))))
         create-o (fn [size]
                    ; for odd
                    (for [x (range (inc (quot size 2)))]
                      (for [y (range size)]
                        (if (and (<= x y) (>= (- (dec size) x) y)) 1 0))))

         ; rotation
         reverse-x (fn [coll]
                     (reverse coll))
         reverse-y (fn [coll]
                     (for [l coll]
                       (reverse l)))
         reverse-xy (fn [coll]
                      (reverse-y (reverse-x coll)))
         replace-xy (fn [coll]
                      (let [vcoll (vec (map vec coll))]
                        (for [x (range (count (first vcoll)))]
                          (for [y (range (count vcoll))]
                            ((vcoll y) x)))))

         shift-b (fn [x y mask]
                   (debug-b (bit-shift-right mask (+ y (* x COLY)))))

         harvestable? (fn [mask]
                        (let [_maskb  (to-b (debug "mask" (padding mask)))
                              _rocksb (to-b rocks)
                              disx (- LOWX (count mask))
                              disy (- COLY (count (first mask)))]
                          (if (> 0 disx)
                            false
                            (some identity
                              (for [i (range (inc disx))
                                    j (range (inc disy))
                                    :let [_shifted_maskb (shift-b i j _maskb)]]
                                (= (bit-and _shifted_maskb _rocksb) _shifted_maskb))))))

         search-e (fn [mask]
                    ; check all pattern of rotation
                    (or (harvestable? mask)
                        (harvestable? (reverse-x mask))
                        (harvestable? (reverse-y mask))
                        (harvestable? (reverse-xy mask))))

         search-o (fn [mask]
                    (let [sym-mask (replace-xy mask)]
                      (or (harvestable? mask)
                          (harvestable? (reverse-x mask))
                          (harvestable? sym-mask)
                          (harvestable? (reverse-y sym-mask)))))



         ;main
         harvest (fn []
                   (max
                     (loop [n COLY]
                       (let [mask-e (create-e n)]
                         (if (search-e mask-e)
                           (countm mask-e)
                           (if (> n 2)
                             (recur (dec n))
                             0))))
                     (loop [n COLY]
                       (if (even? n)
                         (recur (dec n))
                         (let [mask-o (create-o n)]
                           (if (search-o mask-o)
                             (countm mask-o)
                             (if (> n 2)
                               (recur (- 2 n))
                               0)))))))]

     (debug "rocks" rocks)
     ;(debug (create-o 5))
     ;(debug (replace-xy (create-o 5)))
     (let [ret (harvest)]
       (if (zero? ret)
         nil
         ret))))
  [7 3])


(= 10 (__ [15 15 15 15 15]))
(= 15 (__ [1 3 7 15 31]))
(= 3 (__ [3 3]))
(= 6 (__ [17 22 6 14 22]))

; odd
(= 4 (__ [7 3]))
(= 9 (__ [18 7 14 14 6 3]))

; nil
(= nil (__ [21 10 21 10]))
(= nil (__ [0 31 0 31 0]))

; 1. 1のみの最大と思しき三角形つくる
; 2. 答え合わせ
; (= (map #(bit-and %1 %2) org try) try)
; ダメなら減らして 2へ

; maxの長さの位置
; mk-triangle coor length(高さ？)
; check
;





; 角 → x,y反転
111 -> 001
110 -> 011
100 -> 111
; x反転
111 -> 111
110 -> 011
100 -> 001
; y反転
111 -> 100
110 -> 110
100 -> 111

; 角 → x,y反転
111 -> 001
110 -> 011
100 -> 111
; x反転
111 -> 111
110 -> 011
100 -> 001
; y反転
111 -> 100
110 -> 110
100 -> 111

; pattern B
; y 反転
010 -> 000
111 -> 111
000 -> 010

; x,y入れ替え -> x 反転
010 -> 010
011 -> 110
010 -> 010

;2. shift
;move x, y
;-> bit-shift-right x + cal-size * y

;移動可能量 boardの長さ - 辺の長さ →range, for

