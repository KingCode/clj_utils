(ns clj-utils.char
    (:require [clojure.string :as str]))

(defn base16 
"Converts a decimal integer to its base 16 counterpart.
"
[n] (Integer/toString n 16))

(defn unicodes 
"Prints unicode from start to end, with 'width' items per line, each item separated by 'sep'.
 Each item is printed in the format 
    <hex unicode number>: <platform local string representation of unicode char>
"
  ([start end width sep]
    (let [ pairs (partition 1 (map #(str (base16 %) ": " (char %)) (range start end)))
           codes (apply str (apply concat (interpose sep pairs))) ;;get rid of parentheses
           split (.split ^String codes sep)
           lgroups (partition width split)
           lines (map #(apply str (interpose sep %)) lgroups)
           all (interpose "\n" lines)
          ]   
     (println (apply str all))))
  ([] (unicodes 1 65535 8 "  "))
  ([start end] (unicodes start end 8 "  ")))

