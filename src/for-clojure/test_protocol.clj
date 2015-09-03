(ns for-clojure.test-protocol)

(defprotocol MyProt
  "Comment"
  (foo [x])
  (bar [x]))

(defrecord MyRecord []
  MyProt
  (foo [x] "abc"))

(foo (MyRecord.))
; => "abc"
(.foo (MyRecord.))
; => "abc"

(bar (MyRecord.))
; CompilerException java.lang.AbstractMethodError:
;   Method for_clojure/test_protocol/MyRecord.bar()Ljava/lang/Object; is abstract, compiling:(/Users/a14059/src/studing_living_clojure/src/for-clojure/test_protocol.clj:1:35)

(hoge (MyRecord.))
; CompilerException java.lang.RuntimeException:
;   Unable to resolve symbol: hoge in this context, compiling:(/Users/a14059/src/studing_living_clojure/src/for-clojure/test_protocol.clj:17:1)

(defprotocol MyP
  (fuga [_x]))

(defrecord MyR []
  MyP
  (fuga [this] (vec '())))

(fuga (MyR.))

; Fn内で使いたいときは、フルパスで指定せいってことかな