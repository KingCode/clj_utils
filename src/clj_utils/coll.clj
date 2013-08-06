(ns clj-utils.coll
  (:use [clj-utils [core :only (thread-it)]])
  (:import (java.util Properties)))


(defn in? 
"Yields true if x is in coll, false otherwise."
[coll x] (if (some #{x} coll) true false))


(defn seq-or-vec?
"Yields true if either (seq? coll) or (vector? coll) is true; false otherwise.
"
[ coll ]
  (or (seq? coll) (vector? coll)))


(defn conj-seq
"Yields a seq of coll elements with x as the last element.
"
[ coll x ] 
  (seq (conj (vec coll) x))) 


(defn subsq
"Yields a new seq from coll with its elements from positions start to end - 1
 inclusively.
 start must be zero-based and positive, and start must be <= end.
"
[ coll start end ]
  (thread-it (range start end)
    (map #(nth coll %) it)))


(defn but-subsq
"Yields a new seq containing the same elements of coll in the same order, except for
 the subsequence from positions start to butend - 1.
 butstart must be zero-based and positive, and butstart must be <= butend.
"
([ coll butstart butend ]
  (concat (subsq coll 0 butstart) (subsq coll butend (count coll))))
([ coll butstart ]
  (but-subsq coll butstart (count coll))))


(defn but-nth
"Yields a seq from coll without the element at n"
[ coll n ]
  (but-subsq coll n (+ n 1)))


(defn replace
"Yields a new seq from coll with r and rs replacing elements from start to end - 1 in coll,
 in the returned seq.
"
[ coll start end  r & rs ]
  (concat
    (subsq coll 0 start)
    [ r ] rs
    (subsq coll end (count coll))))


(defn replace-1
"Yields a new seq from coll with the element at pos replaced by r, rs.
"
[ coll pos r & rs ]
  (->> (concat [coll pos (inc pos) r] rs)
    (apply replace)))


(defn insert
"Yields a new seq from coll with in inserted at pos. All elements
 currently at pos and following are appended after the last inserted element
 in the returned seq.
"
[ coll pos in & ins ]
  (concat
    (subsq coll 0 pos)
    [in] ins
    (subsq coll pos (count coll))))


(defmacro foreach 
"
For each element in coll, invokes body after binding sym to the element
From Clojure Programming by Chas Emerick and friends.
"
[[sym coll] & body]
  `(loop [coll# ~coll]
      (when-let [[~sym & xs#] (seq coll#)]
         ~@body
         (recur xs#))))


(defn javaprops-for
"Constructs and returns a java.util.Properties from a clojure map.
"
[ m ] (let [ p (Properties.)
             ks (keys m)
             vs (vals m)
             _ (doall (map #(.setProperty p %1 %2) ks vs)) ]  p ))
