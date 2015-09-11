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
                     _maxc (apply max (conj (map count _rb) (count _rb)))
                     _v (vec (map #(vec (format (str "%0" _maxc "d")
                                            (Integer/parseInt %)))
                              _rb))]
                 (for [x (range _maxc)]
                   (for [y (range _maxc)]
                     (if (< x (count _v))
                       (Integer/parseInt (str ((_v x) y)))
                       0))))

         debug (fn [target]
                 (println "debug::---")
                 (loop [_c target]
                   (println (apply str (first _c)))
                   (if (not-empty _c)
                     (recur (rest _c))
                     target)))

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

         reverse-x (fn [coll]
                     (reverse coll))
         reverse-y (fn [coll]
                     (for [l coll]
                       (reverse l)))
         reverse-xy (fn[coll]
                      (reverse-y (reverse-x coll)))

         check (fn [mask]
                 (= (map #(bit-and %1 %2) rocks mask) mask))

         countm (fn [mine]
                  (reduce + (filter #(= 1 %) (flatten mine))))

         harvest (fn [mask]
                   (if (check mask)
                     (countm mask)
                     0))
         ]
     ;(debug rocks)
     ;(countm rocks)
     ;(harvest create-e)
     ))
  [15 15 15 15 15])

; 1. 1のみの最大と思しき三角形つくる
; 2. 答え合わせ
; (= (map #(bit-and %1 %2) org try) try)
; ダメなら減らして 2へ

; maxの長さの位置
; mk-triangle coor length(高さ？)
; check
;



; [excellent]

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

