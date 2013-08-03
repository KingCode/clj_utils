(ns clj-utils.repl
    (:use [clj-utils 
            [coll :only (in? foreach)]
            [core :only (thread-it)]]))

(defn show-map
"Prints map entries, one per line.
"
[ m ]
  (foreach [e m]
    (println "\tKEY: " (key e) "\tVAL: " (val e))))

(defmacro use-str 
"Yields a serialized form ready to be eval'ed from the REPL.
 Intended to save having to constantly redeclare multiple use dependencies on
 namespaces at the start of each REPL session. 

 Examples: 
    (use-deps my-proj.core my-proj.util) => \"(do (use 'my-proj.core) (use 'my-proj.util))\"
    (use-deps [my-proj [core][util]] yields \"(do (use '[my-proj [core][util]]))\"
"
[ clause & more ]
  (thread-it
    (map #(str "(use '" %) more) 
    (apply str (interpose " " it))
    (str it ")")
    (concat (str "(do (use '" clause ")") it)
    (apply str it)
    (str it ")")))


(defn use-form
"Converts loading-str to the equivalent form. Use together with use-str
 in order to hoist dependencies onto the REPL:
   user=>(eval (use-form (use-str myproject.mymodule ...)))
"
[ loading-str]
  (read-string loading-str))


(defn show-vars
"Shows vars interned in namespace ns, except for those excluded, defaulting
 to user namespace and known system vars resp.
"
([ ns exclude]
  (let [ interned (ns-interns ns)
         selector #(let [ v (val %)
                          k-str (str (key %)) ]
                     (and (not (in? exclude v))
                          (not (.startsWith k-str "user.proxy$java.lang.Object$"))))
         selected (filter selector interned)
         items (map #(key %) selected) ]
     (foreach [it (sort items)] (println "\t" it))))
([ exclude ]
  (show-vars 'user exclude))
([]
  (show-vars ())))


(defn show-cp
"Prints out system classpath elements, one per line.
"
[]
  (let [ cp (. (System/getProperties) getProperty "java.class.path") ]
    (println (apply str (interpose "\n" (. cp split ":"))))))


(defn cls
"Clears the REPL screen, much like cls/clear in Windows/Unix.
 Prints n-1 form feeds - default is 15.
" 
([ n ] (let [ ls (map (fn[_] (println \formfeed)) (range 1 n))
          _ (doall ls)   ] nil))
([] (cls 16)))
