(ns for-clojure.repl5)


; ----------------#81
(= ((fn [ca cb]
      (set (filter #(contains? cb %) ca))) #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

; [excellent]
#(% %2 (% %2 %3)) clojure.set/difference


; ----------------#82
(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(defn ->seq [str]
  (set (map-indexed #(vector % %2) str)))

(defn chainable? [a b]
  (if (< (count a) (count b))
    (chainable? b a)
    (let [t (= 1 (count (clojure.set/difference (->seq a) (->seq b))))]
      (if t
        t
        (let [rests (clojure.set/difference (set a) (set b))]
          (if (= 1 (count rests))
            (= (-> a (.replace (str (first rests)) "")) b)
            false))))))

(clojure.set/difference (->seq "cot") (->seq "caot"))
(chainable? "cot" "coat")

(defn removerest [x coll]
  (let [t (clojure.set/difference (set coll) (set (cons x '())))]
    (if (empty? t) nil t)))

(defn chainableseq?
  ([coll] (for [x coll]
            (do
              (println "---start " x "! ----")
              (chainableseq? x (removerest x coll)))))
  ([tester targetcoll]
   (if (empty? targetcoll)
     true
     (let [chains (filter #(chainable? % tester) targetcoll)]
       (println "debug:" tester chains targetcoll)
       (if-not (empty? chains)

         (flatten (map #(chainableseq? % (removerest % targetcoll)) chains))

         false)))))

(chainableseq? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})


((fn removenth [coll n]
   (flatten (cons (take n coll) (nthrest coll (inc n))))) [1 2 3] 1)

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



