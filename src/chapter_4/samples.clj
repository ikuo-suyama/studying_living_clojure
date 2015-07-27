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

