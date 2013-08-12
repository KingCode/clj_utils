(ns clj-utils.stackp-test
  (:refer-clojure :exclude [pop peek remove replace])
  (:use clj-utils.stackp)
  (:import (clj_utils.stackp DefaultStack+))
  (:use clojure.test))

(def SP (DefaultStack+.))

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
  (testing "pop, pop-n signatures"
    (let [ v1 [1 2 3 4] v2 [100] ]
       (is (oktv [4, [1 2 3]] (pop SP v1)))
       (is (oktv [100, []] (pop SP v2)))
       (is (okt [[3 4] [1 2]] (pop-n SP v1 2)))  
       (is (okt [[] [1 2 3 4]] (pop-n SP v1 0)))
       (is (okt [[100] []] (pop-n SP v2 1))))))

(deftest remove-test
  (testing "remove, remove-r, remove-rn, remove-rnb signatures"
    (let [ v [1 2 3 4 5] v2 [100] ]
       (is (ok [1 2 3 4] (remove SP v)))
       (is (ok [] (remove SP v2)))

       (is (ok [1 2 3 4] (remove-r SP v 4 5)))
       (is (ok [2 3 4 5] (remove-r SP v 0 1)))
       (is (ok [4 5] (remove-r SP v 0 3)))
       (is (ok [5] (remove-r SP v 0 4)))
       (is (ok [1] (remove-r SP v 1 5)))
       (is (ok [1 2] (remove-r SP v 2 5)))
       (is (ok [1 4 5] (remove-r SP v 1 3)))
       (is (ok [] (remove-r SP v2 0 1)))
       
       (is (ok [1 2 3 4] (remove-rn SP v 4 1)))
       (is (ok [2 3 4 5] (remove-rn SP v 0 1)))
       (is (ok [4 5] (remove-rn SP v 0 3)))
       (is (ok [5] (remove-rn SP v 0 4)))
       (is (ok [1] (remove-rn SP v 1 4)))
       (is (ok [1 2] (remove-rn SP v 2 3)))
       (is (ok [1 4 5] (remove-rn SP v 1 2)))
       (is (ok [] (remove-r SP v 0 5)))

       (is (ok [1 2 3 4] (remove-rn SP v 4)))
       (is (ok [1 2 3] (remove-rn SP v 3)))
       (is (ok [1] (remove-rn SP v 1)))
       (is (ok [] (remove-rn SP v 0)))

       (is (ok [1 2 3 4] (remove-rnb SP v 4 1)))
       (is (ok [2 3 4 5] (remove-rnb SP v 0 1)))
       (is (ok [4 5] (remove-rnb SP v 2 3)))
       (is (ok [5] (remove-rnb SP v 3 4)))
       (is (ok [1] (remove-rnb SP v 4 4)))
       (is (ok [1 2] (remove-rnb SP v 4 3)))
       (is (ok [1 4 5] (remove-rnb SP v 2 2)))
       (is (ok [] (remove-rnb SP v 4 5)))
       (is (ok [] (remove-rnb SP v2 0 1))))))

(deftest peek-test
  (testing "peek, peek-n, peek-nb, peek-r, peek-rn, peek-rnb signatures" 
    (let [ v [1 2 3 4 5] v2 [100] ]
       (is (= 5 (peek SP v)))
       (is (= 100 (peek SP v2)))
       
       (is (ok [5] (peek-n SP v 1)))
       (is (ok [4 5] (peek-n SP v 2)))
       (is (ok [1 2 3 4 5] (peek-n SP v 5)))
       (is (ok [100] (peek-n SP v2 1)))
       
       (is (ok [5] (peek-nb SP v 1)))
       (is (ok [5 4] (peek-nb SP v 2)))
       (is (ok [5 4 3 2 1] (peek-nb SP v 5)))
       (is (ok [100] (peek-nb SP v2 1)))

       (is (ok [5] (peek-r SP v 4 5)))
       (is (ok [4 5] (peek-r SP v 3 5)))
       (is (ok [1 2 3 4 5] (peek-r SP v 0 5)))
       (is (ok [100] (peek-r SP v2 0 1)))

       (is (ok [5] (peek-rn SP v 4 1)))
       (is (ok [4 5] (peek-rn SP v 3 2)))
       (is (ok [1 2 3 4 5] (peek-rn SP v 0 5)))
       (is (ok [2 3 4] (peek-rn SP v 1 3)))
       (is (ok [1] (peek-rn SP v 0 1)))
       (is (ok [100] (peek-rn SP v2 0 1)))

       (is (ok [5] (peek-rnb SP v 4 1)))
       (is (ok [5 4] (peek-rnb SP v 4 2)))
       (is (ok [5 4 3 2 1] (peek-rnb SP v 4 5)))  
       (is (ok [4 3 2] (peek-rnb SP v 3 3)))
       (is (ok [1] (peek-rnb SP v 0 1)))
       (is (ok [100] (peek-rnb SP v2 0 1))))))

(deftest but-test
  (testing "but, but-n, but-r, but-rn, but-rnb signatures"
    (let [ v [1 2 3 4 5] v2 [100] ]
       (is (ok [1 2 3 4] (but SP v)))
       (is (ok [1 2 3 4] (but SP v 4)))
       (is (ok [1 2 3 5] (but SP v 3)))
       (is (ok [] (but SP v2)))
       (is (ok [] (but SP v2 0)))

       (is (ok [1 2 3 4] (but SP v 4)))
       (is (ok [1 2 4 5] (but SP v 2)))

       (is (ok [1 2 3 4] (but-n SP v 1)))
       (is (ok [1 2] (but-n SP v 3)))
       (is (ok [1] (but-n SP v 4)))
       (is (ok [] (but-n SP v 5)))

       (is (ok [1 2 3 4] (but-r SP v 4 5)))
       (is (ok [1 2 3] (but-r SP v 3)))
       (is (ok [1 2] (but-rn SP v 2 3)))
       (is (ok [5] (but-rnb SP v 3 4))))))

(deftest insert-test
  (testing "insert insert-b signatures"
    (let [ v [1 2 3 4 5] v2 [100] ]
       (is (ok [1 2 :a :b 3 4 5] (insert SP v 2 [:a :b])))
       (is (ok [:a 1 2 3 4 5] (insert SP v 0 [:a])))
       (is (ok [1 2 3 4 5 :a :b] (insert SP v 5 [:a :b])))
       (is (ok [1 2 3 4 5] (insert SP v 2 [])))
       (is (ok [1 2 3 4 5] (insert SP v 0 [])))
       (is (ok [1 2 3 4 5] (insert SP v 4 [])))
       (is (ok [:a 100] (insert SP v2 0 [:a])))
       (is (ok [100 :a :b :c] (insert SP v2 1 [:a :b :c])))

       (is (ok [1 2 3 :b :a 4 5] (insert-b SP v 3 [:a :b])))
       (is (ok [:b :a 1 2 3 4 5] (insert-b SP v 0 [:a :b])))
       (is (ok [1 2 3 4 5 :b :a] (insert-b SP v 5 [:a :b])))
       (is (ok [:b :a 100] (insert-b SP v2 0 [:a :b])))
       (is (ok [100 :b :a] (insert-b SP v2 1 [:a :b]))))))
       
(deftest replace-test
  (testing "replace, replace-b, replace-n, replace-r, replace-rnb signatures"
    (let [ v [1 2 3 4 5] v2 [100]]
      (is (ok [1 2 :a 4 5] (replace SP v 2 [:a])))
      (is (ok [1 2 3 :a :b :c 5] (replace SP v 3 [:a :b :c])))
      (is (ok [1 2 3 5] (replace SP v 3 [])))
      (is (ok [:a 2 3 4 5] (replace SP v 0 [:a])))
      (is (ok [2 3 4 5] (replace SP v 0 [])))
      (is (ok [1 2 3 4 5 :a :b :c :d] (replace SP v [:a :b :c :d])))
      
      (is (ok [1 2 3 :b :a 5] (replace-b SP v 3 [:a :b])))
      (is (ok [1 2 3 4 :b :a] (replace-b SP v 4 [:a :b])))
      (is (ok [:a 2 3 4 5] (replace-b SP v 0 [:a])))
      (is (ok [:b :a 2 3 4 5] (replace-b SP v 0 [:a :b])))
      (is (ok [2 3 4 5] (replace-b SP v 0 [])))
      (is (ok [1 2 4 5] (replace-b SP v 2 [])))
      (is (ok [1 2 3 4] (replace-b SP v 4 [])))

      (is (ok [1 2 3 4 :a] (replace-n SP v 1 [:a])))
      (is (ok [1 2 3 :a :b] (replace-n SP v 2 [:a :b])))
      (is (ok [:a] (replace-n SP v 5 [:a])))
      (is (ok [] (replace-n SP v 5 [])))
      (is (ok [:a :b] (replace-n SP v2 1 [:a :b])))
      (is (ok [] (replace-n SP v2 1 [])))
      (is (ok [100] (replace-n SP v2 0 [:a :b :c])))

      (is (ok [1 2 :a :b :c 5] (replace-r SP v 2 4 [:a :b :c])))
      (is (ok [1 :a 5] (replace-r SP v 1 4 [:a])))
      (is (ok [:a :b 4 5] (replace-r SP v 0 3 [:a :b])))
      (is (ok [1 2 3 4 :a :b] (replace-r SP v 4 5 [:a :b])))
      (is (ok [1 2] (replace-r SP v 2 5 [])))
      (is (ok [1 2 3 4 5] (replace-r SP v 5 5 [])))
      (is (ok [3 4 5] (replace-r SP v 0 2 [])))
      (is (ok [:a :b] (replace-r SP v2 0 1 [:a :b])))

      (is (ok [1 2 3 :a :b] (replace-rn SP v 3 2 [:a :b])))
      (is (ok [1 :a 5] (replace-rn SP v 1 3 [:a])))
      (is (ok [:a :b :c] (replace-rn SP v 0 5 [:a :b :c])))
      (is (ok [] (replace-rn SP v 0 5 [])))
      (is (ok [:a :b] (replace-rn SP v2 0 1 [:a :b])))
      (is (ok [100] (replace-rn SP v2 0 0 [:a])))
      
      (is (ok [1 2 3 :b :a] (replace-rnb SP v 4 2 [:a :b])))
      (is (ok [1 :a] (replace-rnb SP v 4 4 [:a])))
      (is (ok [1 :b :a] (replace-rnb SP v 4 4 [:a :b])))
      (is (ok [:c :b :a] (replace-rnb SP v 4 5 [:a :b :c])))
      (is (ok [4 5] (replace-rnb SP v 2 3 [])))
      (is (ok [] (replace-rnb SP v 4 5 [])))
      (is (ok [:b :a] (replace-rnb SP v2 0 1 [:a :b]))))))

(deftest test-push
  (testing "push signatures"
    (let [ v [1 2 3] v2 [100] ]
      (is (ok [1 2 3 :a] (push SP v [:a])))
      (is (ok [1 2 3 :a :b :c] (push SP v [:a :b :c])))
      (is (ok [1 2 3] (push SP v [])))
      (is (ok [100 :a :b] (push SP v2 [:a :b])))
      (is (ok [100] (push SP v2 [])))
      )))


