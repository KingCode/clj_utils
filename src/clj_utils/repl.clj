(ns clj-utils.repl)

(defn cls
"Clears the repl screen output by printing n form feeds. Default is 15"
  ([ n ] (dorun (map (fn [_] (println \formfeed)) (range 1 (inc n)))))
  ([] (cls 15)))
