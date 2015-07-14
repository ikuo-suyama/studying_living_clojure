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
(cons 4 5)

(cons 4 nil)
(cons 4 '(4 5))

; vector better index access than list

; common function of collections
(count [1 3 4 5])

(conj [1 3 4] 5)
;=> added at the end [1 3 4 5]
(conj '(1 3 4) 5)
;=> added at first (5 1 3 4)

;map
(get {:key1 "val" :key2 "val2"} :key2)
;=> "val2"

; set default value
(get {:key1 "val" :key2 "val2"} :key3 "not found")
=> "not found"

; getting value using key as the funtion
(:key1 {:key1 "test" :key2 "test2"})
;=> "test"

;this is Error:
;  java.lang.String cannot be cast to clojure.lang.IFn
("key1" {"key1" "test" :key2 "test2"})


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

