(ns clj-utils.char)

(defn base16 [n] (Integer/toString n 16))

(defn unicodes 
  ([start end width sep]
    (let [ pairs (partition 1 (map #(str (base16 %) ": " (char %)) (range start end)))
           codes (apply str (apply concat (interpose sep pairs))) ;;get rid of parentheses
           split (.split codes sep)
           lgroups (partition width split)
           lines (map #(apply str (interpose sep %)) lgroups)
           all (interpose "\n" lines)
          ]   
     (apply str all)))
  ([] (unicodes 1 65535 8 "  "))
  ([start end] (unicodes start end 8 "  ")))

