(ns clj-utils.coll-test
  (:use clj-utils.coll)
  (:use clojure.test))

(deftest test-in?
  (testing "Verifies that true is returned iff x is in coll"
    ( let [ c1 [1 2 3 4]
            c2 [ "one" 2 3 "four"]
            c3 (list [:a 1 2] [:b 3 4]) 
            c4 (seq '(("mule" 2 3) ("donkey" 4 5))) ]
        (is (in? c1 2)) 
        (is (not (in? c1 5)))
        (is (in? c2 "one"))
        (is (in? c3 [:b 3 4]))
        (is (in? c3 '(:a 1 2)))
        (is (in? c4 '("mule" 2 3)))
        (is (in? c4 '("donkey" 4 5)))
        (is (not (in? c4 '("mule" 3 2)))))))

(deftest subsq-test
  (testing "Should extract a subsequence from sequentials"
    (let [ li '(1 2 3 4 5) vc [1 2 3 4 5] ]
        (is (= '(1 2) (subsq li 0 2)))
        (is (= '(1 2) (subsq vc 0 2)))
        (is (= '(2 3 4) (subsq li 1 4)))
        (is (= '(5) (subsq li 4 5)))
        (is (= '(1 2 3 4 5) (subsq li 0 5)))
        (is (= '(1) (subsq li 0 1)))
        (is (= '()  (subsq li 0 0)))
        (is (= '() (subsq li 4 4))))))


(deftest but-subsq-test
  (testing "Should extract elements except those in a specified range"
    (let [ li '(1 2 3 4 5) vc [1 2 3 4 5]]
        (is (= '(1 2) (but-subsq li 2 5)))
        (is (= '(1 2) (but-subsq vc 2 5)))
        (is (= '(1 4 5) (but-subsq li 1 3)))
        (is (= '(1) (but-subsq li 1 5)))
        (is (= '() (but-subsq li 0 5)))
        (is (= '(5) (but-subsq li 0 4))))))

(deftest replace-with-test
  (testing "Should replace a single element with one or more others, at the indicated position
            in a non-empty seq"
    (let [ li '(1 2 3 4 5)]
        (is (= '(1 2 :three 4 5) (replace-with li 2 :three)))
        (is (= '(:one 2 3 4 5) (replace-with li 0 :one)))
        (is (= '(1 2 3 4 :five) (replace-with li 4 :five)))
        (is (= '(1 2 :a :b :c 4 5) (replace-with li 2 :a :b :c)))
        (is (= '(:one) (replace-with '(1) 0 :one))))))


(deftest insert-test
  (testing "Should insert one or more elements at a specified position, preserving the rest of coll"
    (let [ li '(1 2 3 4 5) ]
        (is (= '(1 :a 2 3 4 5) (insert li 1 :a)))
        (is (= '(1 :a :b 2 3 4 5) (insert li 1 :a :b)))
        (is (= '(:a :b :c 1 2 3 4 5) (insert li 0 :a :b :c)))
        (is (= '(1 2 3 4 :a 5) (insert li 4 :a)))
        (is (= '(1 2 3 4 :a :b 5) (insert li 4 :a :b)))
        (is (= '(1) (insert () 0 1)))
        (is (= '(1 2 3 4 5 6) (insert li 5 6)))
        (is (= '(1 2 3 4 5 6 7) (insert li 5 6 7))))))

(deftest but-nth-test
  (testing "Should remove a single element"
    (let [ li '(1 2 3 4 5)]
        (is (= '(1 2 4 5) (but-nth li 2)))
        (is (= '(2 3 4 5) (but-nth li 0)))
        (is (= '(1 2 3 4) (but-nth li 4))))))

