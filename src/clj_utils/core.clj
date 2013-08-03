(ns clj-utils.core)

;; dummy def for rebinding within thread-it 
(def it "for use by thread-it")


(defmacro thread-it 
"A thread-anywhere (anaphoric) macro, makes life a lot easier
 in chained invocations where thread-first/last would require
 too much gymnastics..

 From Clojure In Action, 1st ed. by Amit Rahore.
"
[& [first-expr & rest-expr]]
  (if (empty? rest-expr) first-expr
            `(let [~'it ~first-expr]
                (thread-it ~@rest-expr))))


(defn str2int
"Parses an integer from a string.
"
[ s ]
  (Integer/parseInt s))


(defn classpath
"Shows system classpath elements"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (apply str (interpose "\n" (. cp split ":")))))
