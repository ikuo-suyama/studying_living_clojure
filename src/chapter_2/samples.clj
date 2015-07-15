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
(defn recur-combine-string [comb-str target-list]
  ((fn ! [output recurring-list]
     (if (empty? recurring-list)
       (str comb-str output)
       (! (str comb-str (first recurring-list))
          (rest recurring-list)))) "" target-list))

;    (str comb-str (first target-list)))

;  ((fn [x x2] (str x x2)) comb-str (first target-list)))

  (recur-combine-string " " ["test1" "test2"])