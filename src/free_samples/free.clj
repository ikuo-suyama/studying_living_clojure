(ns free-samples.free)

; macro -> ... method chain
(-> 1 (+ 1))
(macroexpand '(-> 1 (+ 1)))
;=> (+ 1 1)

(-> "up"
    .toUpperCase
    .toLowerCase)
(macroexpand '(-> "up" .toUpperCase .toLowerCase))
;=> (. (.toUpperCase "up") toLowerCase)


(doto "a b c d"
  .toUpperCase
  (.replace "A" "E")
  .split
  first)
