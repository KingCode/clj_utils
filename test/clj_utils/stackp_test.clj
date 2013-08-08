(ns clj-utils.stackp-test
  (:use clj-utils.stackp)
  (:use clojure.test))

(def SP default-stack+)

(defn okt 
"Verifies tuple test output, i.e. that val = refval, coll = refvec and expected types"
([ [refval refvec] [val coll] val-is-vector?]
    (and (= [refval refvec] [val coll]) 
         (if val-is-vector? (vector? val) true)
         (vector? coll)))
( [ rf tested ]
  (okt rf tested true)))


(defn oktv 
"Short-hand for (ok rf tested false)"
[ rf tested ]
  (okt rf tested false))


(defn ok
"Validates vector test outputs, i.e. that rf = tested and that tested is a vector"
[ rf tested ]
  (and (= rf tested) (vector? tested)))


(deftest ok-test
  (testing "Should correctly verify a tuple's contents"
    (let [ r1 [4 [1 2 3]] t1t [4 [1 2 3]]
                          t1f1 [2 [1 2 3]]
                          t1f2 [4 '(1 2 3)]
           r2 [[4 5] [1 2 3]]
                          t2t [[4 5] [1 2 3]]
                          t2f1 [[4]  [1 2 3]]
                          t2f2 [[4 6] [1 2]]
                          t2f3 [[4 5] [1 2]]
                          t2f4 ['(4 5) [1 2 3]]
                          t2f5 ['(4 5) '(1 2 3)]
                          t2f6 [[4 5] '(1 2 3)] 
        ]
    (is (okt r1 t1t false))
    (is (not (okt r1 t1f1 false)))
    (is (not (okt r1 t1f2 false)))

    (is (okt r2 t2t))
    (is (not (okt r2 t2f1)))
    (is (not (okt r2 t2f2)))
    (is (not (okt r2 t2f3)))
    (is (not (okt r2 t2f4)))
    (is (not (okt r2 t2f5)))
    (is (not (okt r2 t2f6))))))


(deftest pop-test
  (testing "pop signatures"
    (let [ v [1 2 3 4]]
       (is (oktv [4, [1 2 3]] (pop SP v)))
       (is (okt [[3 4] [1 2]] (pop-n SP v 2)))  
       (is (okt [[] [1 2 3 4]] (pop-n SP v 0))))))
       
