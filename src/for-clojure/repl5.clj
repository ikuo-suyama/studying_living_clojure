(ns for-clojure.repl5)


; ----------------#81
(= ((fn [ca cb]
      (set (filter #(contains? cb %) ca))) #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

; [excellent]
#(% %2 (% %2 %3)) clojure.set/difference


; ----------------#82
(= true (
          (fn _ [origin]
           (letfn [
                   (chainableseq?
                     ([coll] (reduce #(or % %2)
                                     (for [x coll]
                                       (do
                                         (println "---start " x "! ----")
                                         (reduce #(or % %2) (chainableseq? x (removerest x coll)))))))

                     ([tester targetcoll]
                      (if (empty? targetcoll)
                        true
                        (let [chains (filter #(chainable? % tester) targetcoll)]
                          (println "debug:" tester chains targetcoll)
                          (if-not (empty? chains)
                            (flatten (map #(chainableseq? % (removerest % targetcoll)) chains))
                            false)))))

                   (removerest [x coll]
                               (let [t (clojure.set/difference (set coll) (set (cons x '())))]
                                 (if (empty? t) nil t)))

                   (chainable? [a b]
                               (cond
                                 (< (count a) (count b)) (chainable? b a)
                                 (< 1 (- (count a) (count b))) false

                                 ;pattern1 replace
                                 (= 1 (count (clojure.set/difference (->seq a) (->seq b)))) true

                                 ;pattern2 added top or last
                                 (= 1 (count (-> a (.replace b "")))) true

                                 ;pattern3 inserted
                                 :else
                                 (let [rests (clojure.set/difference (set a) (set b))]
                                   (if (= 1 (count rests))
                                     (= (-> a (.replace (str (first rests)) "")) b)
                                     false))))

                   (->seq [str]
                          (set (map-indexed #(vector % %2) str)))]
             (chainableseq? origin)))
          #{"share" "hares" "shares" "hare" "are"}))


; parts
(defn ->seq [str]
  (set (map-indexed #(vector % %2) str)))

;(defn chainable? [a b]
;  (cond
;    (< (count a) (count b)) (chainable? b a)
;    (< 1 (- (count a) (count b))) false
;    :else
;    (let [t (= 1 (count (clojure.set/difference (->seq a) (->seq b))))]
;      (if t
;        t
;        (let [rests (clojure.set/difference (set a) (set b))]
;          (if (= 1 (count rests))
;            (= (-> a (.replace (str (first rests)) "")) b)
;            false))))))

(defn chainable? [a b]
  (cond
    (< (count a) (count b)) (chainable? b a)
    (< 1 (- (count a) (count b))) false

    ;pattern1 replace
    (= 1 (count (clojure.set/difference (->seq a) (->seq b)))) true

    ;pattern2 added top or last
    (= 1 (count (-> a (.replace b "")))) true

    ;pattern3 inserted
    :else
    (let [rests (clojure.set/difference (set a) (set b))]
      (if (= 1 (count rests))
        (= (-> a (.replace (str (first rests)) "")) b)
        false))))


(chainable? "hares" "shares")

(defn removerest [x coll]
  (let [t (clojure.set/difference (set coll) (set (cons x '())))]
    (if (empty? t) nil t)))

(defn chainableseq?
  ([coll] (reduce #(or % %2)
                  (for [x coll]
                    (do
                      (println "---start " x "! ----")
                      (reduce #(or % %2) (chainableseq? x (removerest x coll)))))))

  ([tester targetcoll]
   (if (empty? targetcoll)
     true
     (let [chains (filter #(chainable? % tester) targetcoll)]
       (println "debug:" tester chains targetcoll)
       (if-not (empty? chains)
         (flatten (map #(chainableseq? % (removerest % targetcoll)) chains))
         false)))))

(chainableseq? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})

(fn _ [origin]
  (letfn [
          (chainableseq?
            ([coll] (reduce #(or % %2)
                            (for [x coll]
                              (do
                                (println "---start " x "! ----")
                                (reduce #(or % %2) (chainableseq? x (removerest x coll)))))))

            ([tester targetcoll]
             (if (empty? targetcoll)
               true
               (let [chains (filter #(chainable? % tester) targetcoll)]
                 (println "debug:" tester chains targetcoll)
                 (if-not (empty? chains)
                   (flatten (map #(chainableseq? % (removerest % targetcoll)) chains))
                   false)))))

          (removerest [x coll]
                      (let [t (clojure.set/difference (set coll) (set (cons x '())))]
                        (if (empty? t) nil t)))

          (chainable? [a b]
                      (if (< (count a) (count b))
                        (chainable? b a)
                        (let [t (= 1 (count (clojure.set/difference (->seq a) (->seq b))))]
                          (if t
                            t
                            (let [rests (clojure.set/difference (set a) (set b))]
                              (if (= 1 (count rests))
                                (= (-> a (.replace (str (first rests)) "")) b)
                                false))))))

          (->seq [str]
                 (set (map-indexed #(vector % %2) str)))]
    (chainableseq? origin)
    ))





; combination?
; permutation?
; sample partial
((fn partial

   ([coll] (partition-all (count coll) (flatten (partial [] coll))))

   ([current coll]
    (letfn [(_removenth [coll n]
                       (flatten (cons (take n coll) (nthrest coll (inc n)))))]
      (if (empty? coll)
        current
        (for [i (range (count coll))]
          (partial (conj current (nth coll i)) (_removenth coll i)))))
     ))
  [1 2 3 4 5])



; ----------------#83
(= false ((fn [& coll]
            (println coll)
            (reduce #(or % (not %2)) false coll)) true false true))

(= true (__ false true false))

((fn [& coll]
   (cond
     (reduce #(and % (not %2)) true coll) false
     (reduce #(or % (not %2)) false coll) true
     :else false))
  true false)

; [excellent]
; not=


; ----------------#84
(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (__ more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))


((fn trans [coll]
   (letfn [(brel [_r x]
                 (loop [ret []
                        key (get _r x)
                        val (get _r key)]
                   (println "debug:" x key val)
                   (if val
                     (recur (conj ret [x val]) val (get _r val))
                     ret
                     )))]
     (let [_map (reduce merge (map #(apply hash-map %) coll))]
       (reduce #(if (coll? (first %2))
                 (apply conj %1 %2)
                 (conj %1 %2))
               #{}
               (for [c coll]
                 (let [_brel (brel _map (first c))]
                   (if (empty? _brel)
                     c
                     (conj _brel c))))))))
  #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})

; [excellent]
(fn f [s]
  (let [r (apply conj s (remove nil? (for [a s b s]
                                       (if (= (second a) (first b)) [(first a) (second b)]))))] (if (= r s) r (f r))))

; ----------------#85
; powerset::べき集合
(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})

; できるけど遅い
((fn pwset [coll]
   (letfn [(subs [_coll] (if (empty? _coll)
                           _coll
                           (for [x _coll]
                             (let [sub (clojure.set/difference _coll (hash-set x))]
                               (cons sub (subs sub))))))]
     (set (conj (distinct (flatten (subs coll))) coll))))
  #{1 2 3 4})
;#{#{4 3} #{1 4 3 2} #{} #{3} #{4 3 2} #{2} #{1 4} #{1} #{4 2} #{1 3 2} #{1 3} #{1 2} #{1 4 3} #{4} #{3 2} #{1 4 2}}

((fn [coll x]
   (apply conj coll (for [v coll]
           (conj v x)))) #{#{} #{1}} 2)

(reduce #(apply conj %1
                (for [v %1]
                  (conj v %2))) #{#{}} #{1 2 3})


; [excellent] for mine!!

; ----------------#86
; take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number.
(= (__ 7) true)

((fn happy? [n]
   (letfn [(calc-next [n]
                      (reduce + (map #(* (Long. (str %)) (Long. (str %))) (set (str n)))))]

     (loop [x (calc-next n)
            ret #{n}]
       (if (contains? ret x)
         (= 1 x)
         (recur (calc-next x) (conj ret x))))))
  7)

((fn calcNext [n]
   (reduce + (map #(* (Long. (str %)) (Long. (str %))) (set (str n))))) 7)


; ----------------#87
(#(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1)) #{1 2 3 4 5 6} #{1 3 5 7})


; ----------------#88
((fn graph? [coll]
   (letfn [(nodes [_coll]
                  (distinct (flatten _coll)))

           (edge? [node tuple]
                  (contains? (set tuple) node))

           (next-node [node edge]
                 (if (apply distinct? edge)
                   (first (disj (set edge) node))
                   (first edge)))

           (_remove [coll x]
                    (let [n (.indexOf coll x)]
                      (cond
                        (= 0 n) (vec (rest coll))
                        (= (count coll) (inc n)) (vec (butlast coll))
                        :else (apply conj (subvec coll 0 n) (subvec coll (inc n))))))

           (path? [node edges]
                  (if (empty? edges)
                    true
                    (loop [_current edges]
                      (println "debug!:" node edges _current)
                      (if (empty? _current)
                        false
                        (let [e (first _current)]
                          (if (edge? node e)
                            (path? (next-node node e) (_remove edges e))
                            (recur (rest _current))))
                        ))))]

     (loop [_nodes (nodes coll)]
       (println "----- start " (first _nodes) " -----")
       (if (path? (first _nodes) coll)
         true
         (if-not (empty? _nodes)
           (recur (rest _nodes))
           false)))
     ))
  [[:a :b] [:a :c] [:c :b] [:a :e]
   [:b :e] [:a :d] [:b :d] [:c :e]
   [:d :e] [:c :f] [:d :f]])


; ----------------#89
(= (__ #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})

((fn [a b]
   (set (for [x a
          y b]
      (conj [] x y)))) #{1 2 3} #{4 5})

