(ns clj-utils.core-test
  (:use clojure.test)
  (:use clj-utils.core))


(deftest macro-call-test
  (testing "Should yield a eval-ready form from a for with a macro in function position"
     (is (= 1 (eval (macro-call '(let [x 0] (inc x))))))

     ;;NOTE: using a regular quote here, and so have to fully specify -> is-case 
     (let [ form '(let [x "x"] (condp clj-utils.core/is-case x #(not (number? %)) "not a number" odd? "odd" even? "even")) ]
        (is (= "not a number" (eval (macro-call form)))))

     ;;NOTE: using syntax-quote
     (let [ form `(let [~'x 1] (condp is-case ~'x #(not (number? %)) "not a number" odd? "odd" even? "even")) ]
        (is (= "odd" (eval (macro-call form)))))

     (let [ form `(let [~'x 0] (condp is-case ~'x #(not (number? %)) "not a number" odd? "odd" even? "even")) ]
        (is (= "even" (eval (macro-call form)))))
))

