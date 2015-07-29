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



(defmulti eat-mushroom (fn [height]
                         (if (< height 3)
                           :grop
                           :shrink)))

(defmethod eat-mushroom :grow [_]
  "Eat the right side to grow.")
(defmethod eat-mushroom :shrink [_]
  "Eat the left side to shink.")

(eat-mushroom 5)
(eat-mushroom 2)

