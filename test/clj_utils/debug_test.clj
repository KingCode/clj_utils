(ns clj-utils.debug-test
  (:use clj-utils.debug)
  (:use clojure.test))

(deftest  func-name-test
    "Should return a function's name"
    (is (= "+" (func-name +)))
    (is (= "map" (func-name map))))


(deftest  around-advice-test-1
    "Should execute advice accurately and still return the value accurately and transparently"
    (let [ stored-val (atom 0)
           swapper (fn [ & args] (swap! stored-val #(inc %)))
           with-advice (around-advice + swapper swapper 10 23) 
           without-advice (+ 10 23) ]
        (is (= without-advice with-advice))
        (is (= 2 @stored-val))))

(deftest  around-advice-test-2
    "Should execute advice accurately and still return the value accurately and transparently"
    (let [ stored-in (atom nil)  ; args
           stored-out (atom nil) ; args and yield 
           in-advice (fn [ & args ] (swap! stored-in (fn [_] (str args))))
           out-advice (fn [ & args] (swap! stored-out (fn [_] (str args))))
           with-advice (around-advice + in-advice out-advice 4 7)
           without-advice (+ 4 7) ]
        (is (= without-advice with-advice))
        (is (not (nil? @stored-in)))
        (is (not (nil? @stored-out)))))

(deftest  debug-info-test-1
    "Should print-out debugging information as well as yield advised function's return value accurately"
    (is (= 7 (debug-info + 4 3))))
