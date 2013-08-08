(ns clj-utils.coll
  (:refer-clojure :exclude [replace]) 
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
"Yields a new seq from an indexed collection with its elements from positions start to end - 1
 inclusively.
"
[ coll start end ]
  (thread-it (range start end)
    (map #(nth coll %) it)))


(defn but-subsq
"Yields a new seq containing the elements of an indexed coll order, except for
 those within the range from start to (dec butend) .
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
"Yields a new seq from coll with items replacing elements from start to end - 1 in coll,
 in the returned seq. If there are no replacements itsm the replaced elements are still
 returned from coll.
"
[ coll start end  & items ]
  (cond (>= start end) coll
      :else
      (concat
        (subsq coll 0 start)
        items
        (subsq coll end (count coll)))))


(defn replace-1
"Yields a new seq from coll with the element at pos replaced by each of items.
 If there are no items the replaced items is still removed.
"
[ coll pos & items]
     (apply replace coll pos (inc pos) items))


(defn insert
"Yields a new seq from coll with element at pos replaced by each of  items. 
"
[ coll pos & items]
  (concat
    (subsq coll 0 pos)
    items
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
