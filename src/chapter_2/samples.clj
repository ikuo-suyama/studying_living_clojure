(ns chapter-2.samples)

; not function
(not true)
; => false

; check nil
(nil? nil)
; => true

; nil is false
(not nil)
; => true

; but this is false
(false? nil)
; => false

; equality of label
(= :drink :drink)
; => true

; collection
(= '(:test :test2) [:test :test2])
; => true

; not
(not= :test 1)
;=> true


; -*-*-*-*-*-* Collections -*-*-*-*-*-*
; collection --> implements clojure.lang.IPersistentCollection interface
(seq [1 2 3])

(class (seq [1 2 3]))
;=> clojure.lang.PersistentVector$ChunkedSeq

;every? function -> check every elements in collections
(every? odd? [1 3 5])
;=> true

;we have also "not-any"
(not-any? odd? [2 5])
;=> false

(not-any? odd? [2 4])
;=> true

; some --> return first "true"
(some (fn [x] (< x 3)) [1 2 3 4 5])
;=> true

; to predict set contents
(some #{3} [1 2 2 4 3 4])
; => 3

; return first value of target
(some #{4 2} [1 2 2 4 3 4])
; => 2

; -*-*-*-*-*-* Flow Controll -*-*-*-*-*-*
; if
(if (= :drinkme :drinkme)
  "Fucked Up!"
  "Dont try it")
; => "Fucked Up!"

; let if
(let [try-drink (= :drinkme :drinkme)]
  (if try-drink
    "Fucked Up!"
    "Dont try it"))

; let-if for explain the if sentence meaning
(if-let [try-drink (= :drinkme :drinkme)]
  "Fucked Up!"
  "Dont try it")


; "when" return nil if the case is false
(when-let [would_you_driknk false]
  "Fucked Up"
  "Dont try it")
; -> nil

; "cond" --> using if you have several testing case
(let [bottle "drinkme"]
  (cond
    (= bottle "poison")  "dont touch"
    (= bottle "drinkme") "Fucked Up"))
; => "Fucked Up"

(let [bottle "what else"]
  (cond
    (= bottle "poison")  "dont touch"
    (= bottle "drinkme") "Fucked Up")
    :else "OMG")
; => "OMG"

; case is shortcut of cond only using an "="
(let [bottle "test"]
  (case bottle
    "check" "no"
    "test" "ok"))
; => ok

; partial --> using to curring function
; ready to 1 parameter applied
(defn grow [name direction]
  (if (= direction :small)
    (str name "is growing smaller")
    (str name "is growing bigger")))

(grow "Alice" :small)
; => "Aliceis growing smaller"

; this function can be used separate parameters using partial
((partial grow "Alice") :small)
; => "Aliceis growing smaller"


; comp - using combines other function
(defn toggle-grow [direction]
  (if (= direction :small) :big :small))

(defn comp-grow [direction]
  ((comp (partial grow "Alice") toggle-grow) direction))

(comp-grow :small)
;=> "Aliceis growing bigger"



; -*-*-*-*-*-* Destructuring -*-*-*-*-*-*
; "we can transform our code more understandable with Destructuring
(let [[color size] ["blue" "small"]]
  (str "the " color " door is " size))
;=> "the blue door is small"

; same meaning
(let [x ["blue" "small"]
      color (first x)
      size  (last x)]
  {:color color :size size})

; assin the value of map in let
(let [{flower :flower1 flower2 :flower2}
      {:flower1 "test1" :flower2 "test2"}]
  (str flower " !! " flower2))

; :or and :as
(let [{flower  :flower1
       flower2 :flower2
       flower3 :flower3
       :or     {flower3 "missing"}
       :as     whole-flowers}
      {:flower1 "blue" :flower2 "red"}]
  (str flower " !! " flower2 " " flower3 " " whole-flowers))

; :keys keyword
(let [{:keys [flower1 flower2]}
      {:flower1 "red" :flower2 "blue"}]
  (str "The flowers are " flower1 " and " flower2))

; same as this...?
(let [{flower1 :flower1 flower2 :flower2}
      {:flower1 "red" :flower2 "blue"}]
  (str "The flowers are " flower1 " and " flower2))

; in function
(defn flower-colors [{:keys [flower1 flower2]}]
  (str "The flowers are " flower1 " and " flower2))
(flower-colors {:flower1 "red" :flower2 "blue"})

; complicated pattern
(let [{:keys [param1 param2 param3 param4]
       :or   {param3 "test3" param4 "test4"}}
      {:param1 "test1" :param2 "test2"}]
  (clojure.string/join " " [param1 param2 param3 param4]))
; => "test1 test2 test3 test4"



; -*-*-*-*-*-* Laziness -*-*-*-*-*-*
; take -> return first n item of lazySequrence
; range -> create INFINITE sequence
(take 5 (range))
;=> (0 1 2 3 4)

(class (range 5))
;=> clojure.lang.LazySeq

; repeat also create infinite sequence
(take 5 (repeat "test"))
;=> ("test" "test" "test" "test" "test")

; repeat return a value, repeatedly repeat calling function
(take 10 (repeat (rand-int 10)))
; => (9 9 9 9 9 9 9 9 9 9)

(take 10 (repeatedly #(rand-int 10)))
; => (8 6 4 0 0 7 6 0 3 6)

; creating infinite sequence from sequence
(take 20 (rest (cycle ["test1" "test2"])))



; -*-*-*-*-*-* Reccursion -*-*-*-*-*-*
; reccursive function
(def adjs ["normal"
           "too small"
           "too big"
           "is swiming"])
(defn alice-is [in out]
  (if (empty? in)
    out
    ; calling myself
    (alice-is
      (rest in)
      (conj out
            (str "Alice is " (first in))))))

(alice-is adjs [])

; re-write with loop
(defn alice-is-with-loop [input]
  (loop [in input
         out []]
    (if (empty? in)
      out
      (recur (rest in)
             (conj out
                   (str "Alice is " (first in)))))))
(alice-is-with-loop adjs)

; in general, always use recur when you are doing recursive calls.
(defn recursive-combine-string [comb-str target-list]
  ((fn ! [output recurring-list]
     (if (empty? recurring-list)
       (clojure.string/trim output)
       (! (str output comb-str (first recurring-list))
          (rest recurring-list)))) "" target-list))

(recursive-combine-string " " ["test1" "test2" "test3"])

; -> using recur
; --> @see join ***using StringBuilder...
;(defn combine-string-with-recur [target-list]
;  (if (empty? target-list)
;    " "
;    (recur (rest target-list))))
;
;(combine-string-with-recur ["test1" "test2" "test3"])




; -*-*-*-*-*-* Functional Way of Data Transration -Map and Reduce -*-*-*-*-*-*
; Map
(def animals [:mouse :duc :dodo :lory :eaglet])

(map (fn [target] (str target)) animals)
; => (":mouse" ":duc" ":dodo" ":lory" ":eaglet")
; => this is LazySeq so map can treat infinite sequrence

(take 10 (map #(str %) (cycle animals)))

; make sure the side effects are being executed
; ex. println --> output S.O as side effects
(map #(println %) [1 3 4 5 6])
;1
;3
;4
;5
;6
;=> (nil nil nil nil nil)

; => force evaluation ..?

; we can use map for more than 1 seq,
; map stop shortest sequence when several seq
(map (fn [x y](str x "_" y)) [1 2 4 5] [10 11 12])
;=> ("1_10" "2_11" "4_12")

; we can use cycle in this case
(map (fn [x y](str x "_" y)) [1 2 4 5] (cycle [10 11]))
; => ("1_10" "2_11" "4_10" "5_11")

; Reduce
(reduce + [1 2 3 4 5])

; create new vector
; seq + value
(reduce (fn [r x]
          (if (nil? x)
            r
            (conj r x)))
        []
        [:fu_k nil :f_ck nil])

; combine (value + value)
(reduce (fn [r x] (str r "_" x)) ["a" "b" "c"])

; of cource we cannot use reduce with lazy seq(means infinite)


; complement -> create function return opposite truth value
; filter -> create new sequence only contain fn return ture value
(filter (complement nil?) [:test nil :test2 nil])

; same meanig
(remove nil? [:test nil :test2 nil])

; for
; first arg: bind valiable, second: function for each value
(for [animal [:mouse :duck :lory]]
  (str (name animal)))
; => ("mouse" "duck" "lory")

; nested roop
(for [animal [:mouse :duck :lory]
      color [:red :blue]]
  (str (name animal) (name color)))
; => ("mousered" "mouseblue" "duckred" "duckblue" "loryred" "loryblue")

; nested roop 2
(for [i (take 5 (range))
      j (take 4 (range))]
  (str i "_" j))
; => ("0_0" "0_1" "0_2" "0_3" "1_0" "1_1" "1_2" "1_3" "2_0" "2_1" "2_2" "2_3" "3_0" "3_1" "3_2" "3_3" "4_0" "4_1" "4_2" "4_3")


; with :let and when
(for [animal [:mouse :duck :lory]
      color [:red :blue]
  :let [animal-str (str "animal-" (name animal))
        color-str (str "color-" (name color))]
  :when (= color :blue)]
  (str animal-str "_" color-str))
; => ("animal-mouse_color-blue" "animal-duck_color-blue" "animal-lory_color-blue")

; functions oparete collection
; flatten vec into
(partition-all 3 [1 2 3 4 5 6 7 8 9 10])
; => ((1 2 3) (4 5 6) (7 8 9) (10))

