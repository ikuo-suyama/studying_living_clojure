(ns for-clojure.repl)

; ----------------#19
; ;into でVercot化してからget ...
#(get (into [] %)  (- (count %) 1))

; inspired from last
#(if (next %)
  (recur (next %))
  (first %))

; [excellent]
(comp first reverse)

; ----------------#20
(defn f20 [target]
  ((fn [l s]
     (if (next l)
       (recur (next l) (first l))
       s))
    target ""))

(= (f20 (list 1 2 3 4 5)) 4)

; [excellent]
(comp second reverse)


; ----------------#21
(= (#(get (into [] %1) %2) '(4 5 6 7) 2) 6)
;  -----------------------

; [excellent]
#(first (drop %2 %1))

; ----------------#22
(= (
     #(+ 1 (.lastIndexOf (vec %) (last  (vec %))))
     '(1 2 3 3 1)) 5)

; [excellent]
reduce (fn [n x] (inc n)) 0
#(apply + (map (fn [_] 1) %))


; ----------------#23
;(= (
;     #((fn [new target]
;         (if (next target)
;           (recur (conj new (last target)) (split-at (count target) target))
;           (conj next target))
;         ) [] %)
;     [1 2 3 4 5]) [5 4 3 2 1])

; --> listはconjしたとき先頭に追加される
(= (reduce conj () [1 2 3 4 5]) [5 4 3 2 1])
(= (reverse (sorted-set 5 7 2 7)) '(7 5 2))

; [excellent]
#(into '() %)

(= '(5 4 3 2 1) [5 4 3 2 1])
;=> true!!


; ----------------#26
; Fibonacci Sequence
((fn [max]
  (loop [coll [1 1]]
    (if (= (count coll) max)
      coll
      (recur (conj coll (+ (last coll) (last (drop-last coll)))))))) 3)

; [excellent]
#(take % (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))
; => (map first [[1 1] [1 2] [2 3] [3 5]])


; ----------------#27 sequence is a palindrome?
; (true? (__ "racecar"))
#(= (reverse (into [] %)) (into [] %))

; [excellent]
#(= (vec %) (reverse %))


; ----------------#28
(= (#((fn vecrecr [ret x]
        (if (next x)
          (if (coll? (first x))
            (recur (vecrecr ret (first x)) (drop 1 x))
            (recur (into ret (list (first x))) (drop 1 x)))
          (if (coll? (first x))
            (vecrecr ret (first x))
            (into ret (list (first x))))))
      [] %)
     '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))

; [excellent]
#(filter (complement sequential?) (tree-seq sequential? identity %))

; (complement sequential?) -> (not-sequential)
; (tree-seq sequential? identity [4 [5 6]])
; => ([4 [5 6]] 4 [5 6] 5 6)

; (tree-seq branch? children root)
; children -> １の引数を持つ関数。branchのみ呼ばれる。子供を返す。
; Root -> the root node of the tree.
; identity 自分を返す

; ----------------#29
;(= (__ "HeLlO, WoRlD!") "HLOWRD")
(fn [x] (clojure.string/join "" (filter #(Character/isUpperCase %) (into [] x))))

; [excellent]
#(clojure.string/replace % #"[^A-Z]" "")



; ----------------#30
(= (apply str (#((fn [ret x]
                   (if (next x)
                     (if (= (last ret) (first x))
                       (recur ret (next x))
                       (recur (conj ret (first x)) (next x)))
                     (if (= (last ret) (first x))
                       ret
                       (conj ret (first x)))))
                 [] (seq %)) "Leeeeeerrroyyy")) "Leroy")

((fn [ret x]
  (if (next x)
    (if (= (last ret) (first x))
      (recur ret (next x))
      (recur (conj ret (first x)) (next x)))
    (if (= (last ret) (first x))
      ret
      (conj ret (first x)))))
  [] [1 1 1 2 3 3 2 2 3])

; [excellent]
reduce #(if (= (last %1) %2) %1 (conj %1 %2)) []
; reduce の２回め以降→ %1 ... Functionにかかった値 / %2 残りの配列の最初の値
