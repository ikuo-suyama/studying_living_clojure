(ns chapter-1.samples-test
  (:require [clojure.test :refer :all]
            [chapter-1.samples :refer :all]))

(deftest a-test
  (testing "sample-function return 'test'"
    (is (= (sample-function) "test"))))

(deftest test-a-function
  (testing "the function should return filtered value"
    (is (= '(1 2) (test-filtered [1 2 3 4 5 6])))))


