(ns chapter-6.samples
  (:require [clojure.core.async :as async]))

; channel -> with 10 buffers
; if no args, using main thread and with no buffer
(def tea-channel (async/chan 10))

; sync put & get
(async/>!! tea-channel :cup-of-tea)
;=> true

(async/<!! tea-channel)
;=> :cup-of-tea

(async/close! tea-channel)

;=> async put & async get
(let [tea-channel2 (async/chan)]
  (async/go (async/>! tea-channel :cup-of-tea))
  (async/go (println "Thanks for the " (async/<! tea-channel))))


; async go loop
; if input tea-channel,, print the object
(async/go-loop []
  (if-let [tea (async/<! tea-channel)]
    (println "thanks for the " tea)
    )
  (recur))



