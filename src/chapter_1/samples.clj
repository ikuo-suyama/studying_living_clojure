(ns chapter-1.samples)

; special Simple value of clojure
; Ratios
(/ 1 4)
; => 1/4

; Keywords
:keywords



;Collections
; list
'(1 2 "jam")

; con
; this is Error: shold end with nil
; (cons 4 5)

(cons 4 nil)
(cons 4 '(4 5))

; vector better index access than list

; common function of collections
(count [1 3 4 5])

(conj [1 3 4] 5)
;=> added at the end [1 3 4 5]
(conj '(1 3 4) 5)
;=> added at first (5 1 3 4)

(defn test-maps []
  (
    ;map
    (get {:key1 "val" :key2 "val2"} :key2)
    ;=> "val2"

    ; set default value
    (get {:key1 "val" :key2 "val2"} :key3 "not found")
    ; => "not found"

    ; getting value using key as the funtion
    (:key1 {:key1 "test" :key2 "test2"})
    ;=> "test"

    ;this is Error:
    ;  java.lang.String cannot be cast to clojure.lang.IFn
    ("key1" {"key1" "test" :key2 "test2"})
    ))


; assosicate
(assoc {:key1 "test" :key2 "test"} :key1 "value")
; => {:key2 "test", :key1 "value"}


; dissoc
(dissoc {:key1 "test" :key2 "test"} :key1)
; => {:key2 "test"}

; set --> very useful when you treat NO DUPLICATES data
#{:key1 :key2 :key3}

; union
(clojure.set/union #{:key1 :key2 :key3} #{:key1 :key2 :key4 :key5})
; => #{:key3 :key2 :key4 :key1 :key5}

; difference --> get rid of duplicate elements from aug 1
(clojure.set/difference #{:key1 :key2 :key3} #{:key1 :key2 :key4 :key5})
; => #{:key3}

; intersection --> return shared object
(clojure.set/intersection #{:key1 :key2 :key3} #{:key1 :key2 :key4 :key5})
; => #{:key2 :key1}


; creation of set
; from map
(set {:key1 "test" :key2 "test"})
;=> #{[:key2 "test"] [:key1 "test"]}

; from vector
(set [1 3 4 5])
; => #{1 4 3 5}

; checking
(contains? #{1 4 3 5} 5)
; => true

; also enable to use conj, disj
(disj #{:key1 :key2 :key3} :key3)
; => #{:key2 :key1}



;;; --- Symbols (Valiables?)

; create a var object
; created in current namespace(global vers)
(def developer "Alice")
;=> #'chapter-1.samples/developer

(print developer)
(print chapter-1.samples/developer)
;Alice=> nil

; let --> binding to symbols only in let context
(let [developer "alice in wonderland"] developer)
; => "alice in wonderland"
developer
; => "Alice"


; Functions
(defn sample-function [] "test")

(defn test-filtered [a-vector]
  (filter #(< % 3) a-vector))


; with parameters
(defn sample-function-with-params [jam1 jam2]
  {:name "jam-basket"
   :jam1 jam1
   :jam2 jam2})
(sample-function-with-params "a" "b")
; => {:name "jam-basket", :jam1 "a", :jam2 "b"}

; aliases of defn
((fn [a] (print a)) "test")
; test=> nil

; # is alease of fn and %, %1, %2 ... means parametors of anonymous function
(#(print %) "test")
; test=> nil


; namespace
(ns test.namespace)

*ns*
;=> #<Namespace test.namespace>
; developer
; -> not defined
(defn test-namespase [] (print "HOGE"))

;
chapter-1.samples/developer
; => "Alice"

; normal require
(require 'clojure.set)

; namespase with requrie
(ns test2.namespace
  (:require [test.namespace :as t]))
; enable alias as "t"
(t/test-namespase)

; refer all
(ns test3.namespase
  (:require [test.namespace :refer :all]))
; enable calling the function without namespase
(test-namespase)


