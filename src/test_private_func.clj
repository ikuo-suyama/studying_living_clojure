(ns test-private-func)

(defn- foo [] "Hello")
(defn bar [] (str (foo) " World"))

(bar)

(ns test-another-namespace)
(test-private-func/bar)
(test-private-func/foo)
; -> CompilerException java.lang.IllegalStateException: var: #'test-private-func/foo is not public, compiling: