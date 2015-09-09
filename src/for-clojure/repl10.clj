(ns for-clojure.repl10)

; ----------------#127
(= 10 (__ [15 15 15 15 15]))
; 1111      1111
; 1111      1111
; 1111  ->  1111
; 1111      1111
; 1111      1111

(= 6 (__ [17 22 6 14 22]))
; 10001      10001
; 10110      101*0
; 00110  ->  00**0
; 01110      0***0
; 10110      10110


(= 9 (__ [18 7 14 14 6 3]))
; 10010      10010
; 00111      001*0
; 01110      01**0
; 01110  ->  0***0
; 00110      00**0
; 00011      000*1

((fn triangle [coll]
   (let [rocks (let [_r (for [x coll]
                          (Integer/toBinaryString x))
                     maxc (apply max (map count _r))]
                 (vec (map #(vec (map (fn [d] (Integer/parseInt (str d)))
                                  (vec (format (str "%0" maxc "d")
                                               (Integer/parseInt %)))))
                       _r)))
         MAXX (count rocks)
         MAXY (count (first rocks))
         ]
     rocks))
  [17 22 6 14 22])

; 1. 1のみの最大と思しき三角形つくる
; 2. 答え合わせ
; (= (map #(bit-and %1 %2) org try) try)
; ダメなら減らして 2へ

; maxの長さの位置
; mk-triangle coor length(高さ？)
; check
;



; [excellent]

; 角 → x,y反転（移動後全て鏡像反転を調べる）
1111 -> 0000
1110 -> 0001
1100 -> 0011
1000 -> 0111
0000 -> 1111

; y + shift
1111
1110
1100
1000
0000

; y - shift -> x + shift
1110 -> 0111
1100 -> 0110
1000 -> 0100
0000 -> 0000
0000 -> 0000

; y shift
0000
1110
1100
1000
0000

;移動可能量 boardの長さ - 辺の長さ →range, for