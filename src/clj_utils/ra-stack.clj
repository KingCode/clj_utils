(ns clj-utils.ra-stack
    (:require [clj-utils 
                [coll :as cu-cl]
                [core :as cu-cr]] ))

(defprotocol RAStack
"RandomAccessStack, a stack variant unifying functions working with indexed collections having the same concrete type. 

 Provides the traditional stack with added convenience functions, such as block insertion and replacement
 in random indexes and ranges thereof. This type assumes only indexable collections, i.e. for which (nth coll x)
 returns a value.
 
 Consequently and for performance or other reasons, argument collections and returned values may be persistent, 
 transient, or a host-platform type: implementations should specify whether they are (or expected to be) immutable, 
 thread-safe and/or require isolation, as well as whether the returned collections are the same or any other type
 as the input coll type.

 Naming scheme for some signature families:
    -n => args denote a number of elements starting from the end
    -r => args denote a range using from, to indexes
    -rn => args denote a range from an index, n elements forward
    -rnb => args denote a range from an index, n elements backward
    and so forth.
"
  (pop [this coll] [this coll n]
    "Removes the top (last), or if provided, n elements, and yields the removed value(s) in their orginal order")

  (remove [this coll pos]
    "Removes the element at pos and yields the resulting collection.")

  (remove-r [this coll from] [this coll from to]
    "Removes elements starting at from to the end of coll, or until 'to' if provided.")

  (remove-rn [this coll pos n]
    "Removes n elements starting at pos and yields the resulting coll.")

  (remove-rnb [this coll pos n]
    "Removes n elements starting at pos and going backwards.")

  (peek [this coll] 
    "Yields the value of the last element.") 

  (peek-n [this coll n]
    "Yields a subcoll of the last n elements in their position order")

  (peek-nb [this coll n]
    "Yields a subcoll of the last n elements in their reversed position order")

  (peek-r [this coll from] [this coll from to]
    "Yields a subcoll of elements starting with from and ending at to, 
    or the remainder of coll if to is not provided.")

  (peek-rn [this coll pos]
    "Yields a subcoll of n elements starting at pos.")

  (peek-rnb [this call pos]
    "Yields a subcoll of n elements starting at pos going backwards.")

  (but [this coll] [this coll pos] 
    "Yields coll without the element at pos if provided, or the last otherwise.)

  (but-n [this coll n]
    "Yields a subcoll of all elements except the last n items.")

  (but-r [this coll from] [this coll from to]
    "Yields a subcoll without elements starting at from and up to 'to',
     or all remaining elements if to is not provided.") 

  (but-rn [this coll pos]
    "Yields a subcoll without the n elements starting at pos.")

  (but-rnb [this coll pos]
    "Yields a subcoll without the n elements starting at pos, going backward.")

  (insert [this coll pos & items] 
    "Inserts items at pos and yields the resulting coll") 

  (insert-b [this coll pos & items]
    "Inserts items at pos in their reversed order of argument position, and yields the resulting coll") 

  (replace-r [this coll start items] [this coll start to  & items]
    "Replaces coll elements indexed from start to (dec to) with items in order of their argument position, and yields the 
     resulting coll. If to is not provided all remaining elements after start pos. are replaced.")

  (replace [this coll pos & items]
    "Replaces element at index pos in coll with items.")

  (push [this coll & items] 
    "Pushes items on stack top; appends items to the end of coll"))

(def default-impl
"Stack+ default implementation, uses clojure vectors as input and does not keep state, i.e. a new vector is always
 returned. If inputs are not vectors they are converted.
"
  (let [ vvec #(if (vector? %) % (vec %)) ]
    (reify RAStack
      (pop-n 
         ([ this coll n] 
            (let [ start (- (count coll) n) ]
            (-> (cu-cl/but-subsq coll start)
                vec))))

      (pop
         ([ this coll]
            (-> (butlast coll) vec)))

      (pop-r
         ([ this coll from]
            (-> (cu-cl/but-subsq coll from) vec))
         ([ this coll from to]
            (-> (cu-cl/but-subsq coll from to) vec)))

      (peek-n
        ([ this coll n] 
            (let [ c (vvec coll) siz (count c)]
                (subvec c (- siz n) siz))))

      (peek
        ([ this coll]
            (clojure.core/peek (vvec coll))))

      (peek-r
        ([ this coll from to]
            (subvec (vvec coll) from to))
        ([ this coll from]
           (let [ c (vvec coll) siz (count c)]
             (peek-r c from siz))))

      (peek-rn
        ([ this coll from n]
            (peek-r coll from (+ from n)))) 

      (peek-rnb
        ([ this coll from n]
            (peek-r coll (dec from n) from)))

      (but
        ([ this coll]
          (butlast (vvec coll)))
        ([ this coll n]
          (let [ c (vvec coll) (siz count c)]
            (subvec c (- siz n) siz))))    

      (but-r
        ([ this coll from to]
          (-> (cu-cl/but-subsq coll from to) vec))
        ([ this coll from]
          (-> (cu-cl/but-subsq coll from) vec)))

      (but-nth
        ([ this coll n]
          (-> (cu-cl/but-nth coll n) vec)))

      (insert
        ([ this coll pos & items]
          (->> (apply cu-cl/insert coll pos items)
              vec)))

      (replace-r
        ([ this coll from to & items]
          (->> (apply cu-cl/replace coll from to items)
             vec)))

      (replace
        ([ this coll pos & items]
          (->> (apply cu-cl/replace-1 coll pos items))))


