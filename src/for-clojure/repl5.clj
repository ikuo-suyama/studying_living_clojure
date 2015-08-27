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
; permtation?
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