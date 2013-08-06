(ns clj-utils.pprint
  (:use [clojure.pprint :only (cl-format)]))

(defn table
"Taken from http://clj-me.cgrand.net/, Christophe Grand's blog.
 Given a seq of hash-maps, prints a plaintext table of the values of the hash-maps.
 If passed a list of keys, displays only those keys.  Otherwise displays all the
 keys in the first hash-map in the seq.
"
  ([xs]
     (table xs (keys (first xs))))
  ([xs ks]
     (when (seq xs)
       (let [f (fn [old-widths x]
                 (reduce (fn [new-widths k]
                           (let [length (inc (count (str (k x))))]
                             (if (> length (k new-widths 0))
                               (assoc new-widths k length)
                               new-widths)))
                         old-widths ks))
             widths (reduce f {} (conj xs (zipmap ks ks)))
             total-width (reduce + (vals widths))
             format-string (str "~{"
                                (reduce #(str %1 "~" (%2 widths) "A") "" ks)
                                "~}~%")]
         (cl-format true format-string (map str ks))
         (cl-format true "~{~A~}~%" (repeat total-width \-))
         (doseq [x xs]
           (cl-format true format-string (map x ks)))))))
