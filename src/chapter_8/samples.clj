(ns chapter-8.samples)

(defn hi-queen [name]
  (str name " ,so please your Majesty."))

(defn alice-hi-queen []
  (hi-queen "My Name is Alice"))
(alice-hi-queen)
; => "My Name is Alice. ,so please your Majesty."

; using macro for create function.
(defmacro def-hi-queen [name phrase]
  (list 'defn
        (symbol name)
        []
        ; ' -> マクロ展開時に？評価される
        (list 'hi-queen phrase)))

(def-hi-queen suyama-hi-queen "My name is Suyama")

(suyama-hi-queen)
; => "My name is Suyama. ,so please your Majesty."

(macroexpand-1 '(def-hi-queen suyama-hi-queen "My name is Suyama"))
;=> (defn suyama-hi-queen [] (hi-queen "My name is Suyama"))

(macroexpand '(def-hi-queen suyama-hi-queen "My name is Suyama"))
;=> (def suyama-hi-queen (clojure.core/fn ([] (hi-queen "My name is Suyama"))))

; using ` (code templateing)
(let [x 5]
  '(first [x 2 4]))
;=> (first [x 2 4])

(let [x 5]
  `(first [~x 2 4]))
;=> (clojure.core/first [5 2 4])


(defmacro def-hi-queen2 [name phrase]
  `(defn ~(symbol name) []
     (hi-queen ~phrase)))

(def-hi-queen2 ikuo-hi-queen "My name is Ikuo")
(ikuo-hi-queen)

; -制御構造を即時に実行したくない時
; -カスタムシンタックスを使いたいとき
