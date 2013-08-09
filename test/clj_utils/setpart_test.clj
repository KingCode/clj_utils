(ns clj-utils.setpart-test
    (:use clj-utils.setpart)
    (:use clojure.test))

(deftest partition-coll-test
  (testing "partition-coll-test"
    (let [ coll [1 3 4 5 6 9 10] 
           pred #(= 1 (- %2 %1)) ]
      (is (= #{#{1} #{3 4 5 6} #{9 10}} (partition-coll pred coll) )))))
