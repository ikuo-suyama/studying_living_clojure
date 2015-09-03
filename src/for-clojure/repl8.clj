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
