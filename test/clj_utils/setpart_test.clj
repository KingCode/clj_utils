(ns clj-utils.setpart-test
    (:use clj-utils.setpart)
    (:use clojure.test))

(deftest ppartition-test
  (testing "ppartition-test"
    (let [ coll [1 3 4 5 6 9 10] 
           pred #(= 1 (- %2 %1)) ]
      (is (= #{#{1} #{3 4 5 6} #{9 10}} (ppartition pred coll) )))))
