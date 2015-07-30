(ns chapter-4.samples)

; -*-*-*-*-*-* USING JAVA METHOUDS/Class -*-*-*-*-*-*-*-*-*
; Strings ... java.lang.String
(. "caterpillar" toUpperCase)
;=> "CATERPILLAR"

(.toUpperCase "caterpillar")
;=> "CATERPILLAR"

(.indexOf "chaterpillar" "pillar")
; 6

; same
(. "chaterpillar" indexOf "pillar")

; create instance
(String. "Hi")

; use / if you want to use method directory
(java.net.Inet4Address/getByName "localhost")

; doto macro, allows us to take a Java object and then act on it in succession with a list of operations
(def sb (doto (StringBuffer. "who")
          (.append "are")
          (.append "you")))

(.toString sb)


; -*-*-*-*-*-* Polymorphism -*-*-*-*-*-*-*-*-*
; has small types and many different functions for them.

; using defmulti
(defmulti who-are-you class)

(defmethod who-are-you java.lang.String [input]
  (str "String - who are you? " input))

(defmethod who-are-you clojure.lang.Keyword [input]
  (str "Keyword - who are you? " input))

(defmethod who-are-you :default [input]
  (str "I Don't know - who are you? " input))

(who-are-you :caterpillar)
; => "String - who are you? caterpillar"

(who-are-you "caterpillar")
;=> "Keyword - who are you? :caterpillar"

(who-are-you 1234)
;=> "I Don't know - who are you? 1234"


; Multi-Method
(defmulti eat-mushroom (fn [height]
                         (if (< height 3)
                           :grow
                           :shrink)))

(defmethod eat-mushroom :grow [_]
  "Eat the right side to grow.")
(defmethod eat-mushroom :shrink [_]
  "Eat the left side to shink.")

(eat-mushroom 5)
(eat-mushroom 2)



; Extend-Protocol
(defprotocol BigMushroom
  (eat-mushroom2 [this]))

(extend-protocol BigMushroom
  java.lang.String
  (eat-mushroom2 [this]
    (str (.toUpperCase this) " mmm tasty!"))

  clojure.lang.Keyword
  (eat-mushroom2 [this]
    (case this
      :grow "Eat the right sidde!"
      :shirink "Eat the left side!")))

(eat-mushroom2 :grow)
;=> "Eat the right sidde!"
(eat-mushroom2 "Mushroom")
;=> "MUSHROOM mmm tasty!"


; Def Record (Data Structure)
(defrecord Mushroom [color height])

; create instance
(def regular-mushroom (Mushroom. "white and blue" "2 inches"))
(class regular-mushroom)
(print regular-mushroom)

; get field balues
(.-color regular-mushroom)
; => "white and blue"

; record with protocol
(defprotocol Edible
  (right-side [this])
  (left-side [this]))


; Strategy Pattern
(defrecord WonderlandMushroom [color size]
  Edible
  (right-side [this]
    (str "The " color " bite makes yuo grow bigger"))
  (left-side [this]
    (str "The " color " bite makes yuo grow smaller")))
(defrecord RegularMushroom [color size]
  Edible
  (right-side [this]
    (str "Taste Good"))
  (left-side [this]
    (str "Taste Quite Good")))

(def wonder (WonderlandMushroom. "white" "quite bigger"))
(def regular (RegularMushroom. "white" "quite bigger"))

(. wonder right-side)
;=> "The white bite makes yuo grow bigger"
(.left-side wonder)
;=> "The white bite makes yuo grow smaller"
(left-side wonder)
;=> "The white bite makes yuo grow smaller"

(left-side regular)
;=> "The white bite makes yuo grow smaller"


; this is same meaning ... we should consider about it's really needed before using protocols
; "protocol shold be used sparingly" 倹約して
(defn bite-right-side-by-type [mashroom]
  (if (= (:type mashroom) "wandermashroom")
    "grow bigger"
    "taste good"))

(bite-right-side-by-type {:type "wondermashroom"})





