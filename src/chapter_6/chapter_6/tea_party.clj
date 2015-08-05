(ns chapter-6.chapter-6.tea-party
  (:require [clojure.core.async :as async]))

(def google-tea-service-chain (async/chan 10))
(def yahoo-tea-service-chain (async/chan 10))


(defn random-add []
  (reduce + (conj [] (repeat 1 (rand-int 100000)))))

(defn request-google-tea-service []
  (random-add)
  (async/go
    (async/>! google-tea-service-chain
              "tea-from-google")))

(defn request-yahoo-tea-service []
  (random-add)
  (async/go
    (async/>! yahoo-tea-service-chain
              "tea-from-yahoo")))

;(defn request-tea []
;  (request-google-tea-service)
;  (request-yahoo-tea-service)
;  (async/go (let [[v] (async/alts!
;                    [google-tea-service-chain
;                     yahoo-tea-service-chain])]
;            (println v))))
;
;(request-tea)


;(if you need commandline,,,
(def result-chan (async/chan 10))
(defn request-tea-4-main []
  (request-google-tea-service)
  (request-yahoo-tea-service)
  (async/go (let [[v] (async/alts!
                        [google-tea-service-chain
                         yahoo-tea-service-chain])]
              (async/>! result-chan v))))

(defn -main [&args]
  (println "Requesting tea!")
  (request-tea-4-main)
  (println (async/<!! result-chan)))


;(chapter-6.chapter-6.tea-party/-main [:key])
;Requesting tea!
;tea-from-yahoo
;=> nil
