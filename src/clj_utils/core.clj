(ns clj-utils.core)


(defn in? 
"Yields true if x is in coll, false otherwise."
[coll x] (if (some #{x} coll) true false))

(defn show-cp
"Shows system classpath elements"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (println (apply str (interpose "\n" (. cp split ":"))))))
