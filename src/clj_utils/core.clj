(ns clj-utils.core
    (:use clojure.walk))


(defn funcvar
"Yields the var for a macro or function symbol.
"
[ sym ]
  (->> sym str (str "#'") read-string eval))



(defn macro? 
"Yields true if sym points to a var referencing a macro, false otherwise.
"
[ sym ]
  (-> sym funcvar meta :macro true?))


(defn macro-call
"Yields a form which invokes a macro function with (rest form) as arguments.
"
([ form ]        
  (let [ sym (first form) 
         args (->> (rest form)
                   (walk #(do `(quote ~%)) identity)) ]
    #_(println "REST FORM: " (rest form) ", ARGS: " args)
    `(eval (@(funcvar '~sym) ~form {} ~@args))))
([ sym args ]
    `(eval (@(funcvar '~sym) nil {} ~@args))))


(defn is-case
"Yields the result of (pred expr). A one-arg to two-args predicate adapter intended for
 use with condp.
"
[ pred expr ] (true? (pred expr)))



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


(defn preserve-metadata
"Ensures that the body containing `expr` will carry the metadata
from `&form`. Derived from Clojure Programming, by Emerick/Carper/Grand
"
[&form expr]
  (let [res (with-meta (gensym "res") (meta &form))]
      `(let [~res ~expr]
               ~res)))


(defmacro defmacro-withmeta
"Same as `clojure.core/defmacro`, but preserves user-supplied metadata
(e.g. type hints).Derived from Clojure Programming, by Emerick/Carper/Grand.
"
([ macrocall ]
  (preserve-metadata &form `((fn [] ~macrocall)))))


#_([name doc-string? attr-map? ([params*] body) + attr-map?])
#_(
([] nil)
([x] (preserve-metadata &form x))
([x & next]
    (preserve-metadata 
                 &form 
                `(let [or# ~x]
                    (if or# or# (or ~@next))))))



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
