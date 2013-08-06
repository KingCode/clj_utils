(ns clj-utils.types
    (:require [clj-utils 
                [coll :as cu-cl]
                [core :as cu-cr]] ))

(defprotocol Stack+
"A type intended to facilitate working with indexed collections having the same concrete type. 

 Provides the traditional stack with added convenience functions, such as block insertion and replacement
 in random indexes and ranges thereof. This type assumes only indexable collections, i.e. for which (nth coll x)
 returns a value.
 
 Consequently and for performance or other reasons, argument collections and returned values may be persistent, 
 transient, or a host-platform type: implementations should specify whether they are (or expected to be) immutable, 
 thread-safe and/or require isolation, as well as whether the returned collections are the same or any other type
 as the input coll type.

 Several signatures using a quantity argument, e.g. (pop [... n]) also have a counterpart signature ending with r
 which similarly operate on ranges, e.g. (popr [... from to].
"
  (pop [this coll] [this coll n]
    "Removes the top (last), or if provided, n elements, and yields the removed value(s) in their orginal order")

  (popr [this coll from] [this coll from to]
    "Same as pop, but works with ranges instead of quantities; removal occurs starting at from's index 
     and ends at (dec to) index; if to is not provided, all elements with indexes equal to or greater than from are removed.")

  (peek [this coll] [this coll n]
    "Yields the value of the last element; if n is provided a subcoll of the last n elements in the same order")

  (peekr [this coll from] [this coll from to]
    "Same as peek, but works with ranges instead of quantities; yields a subcoll starting with from and ending at 
     (dec to) index of coll, or the remainder of coll if to is not provided.")

  (but [this coll] [this coll n]
    "Same as clojure.core/butlast, subject to constraints defined in this type and implementations. If n is provided,
     yields a subcoll of all elements except the last n items.")

  (butr [this coll from] [this coll from to]
    "Same as Stack+/but, but works with ranges instead of quantities; yields a subcoll not containing elements
     starting at from and ending at (dec to) indexes, or all remaining elements if to is not provided.") 

  (but-nth [this coll n]
    "Yields the same as coll without item at index n") 

  (insert [this coll start & items] 
    "Inserts items at index start and yields the resulting coll") 

  (replace [this coll start items] [this coll start to  & items]
    "Replaces coll elements indexed from start to (dec to) with items in order of their argument position, and yields the 
     resulting coll. If to is not provided all remaining elements after start pos. are replaced.")

  (push [this coll & items] 
    "Appends items to the end of coll"))

(def default-Stack+
"Stack+ default implementation, uses clojure vectors as input and does not keep state, i.e. a new vector is always
 returned. If inputs are not vectors they are converted.
"
  (let [ vvec #(if (vector? %) % (vec %)) ]
    (reify Stack+
      (pop 
         ([ this coll n] 
            (let [ start (- (count coll) n) ]
            (-> (cu-cl/but-subsq coll start)
                vec))
         ([ this coll]
            (-> (butlast coll) vec)))

      (popr
         ([ this coll from]
            (-> (cu-cl/but-subsq coll from) vec))
         ([ this coll from to]
            (-> (cu-cl/but-subsq coll from to) vec)))

      (peek
        ([ this coll n] 
            (let [ c (vvec coll) siz (count c)]
                (subvec c (- siz n) siz)))
        ([ this coll]
            (clojure.core/peek (vvec coll))))

      (peekr
        ([ this coll from to]
            (subvec (vvec coll) from to))
        ([ this coll from]
           (let [ c (vvec coll) siz (count c)]
             (peekr c from siz))))

      (but
        ([ this coll]
          (butlast (vvec coll)))
        ([ this coll n]
          (let [ c (vvec coll) (siz count c)]
            (subvec c (- siz n) siz))))    

      (butr
        ([ this coll from to]
          (-> (cu-cl/but-subsq coll from to) vec))
        ([ this coll from]
          (-> (cu-cl/but-subsq coll from) vec)))


      (but-nth
        ([ this coll n]
          (-> (cu-cl/but-nth coll n) vec)))


      (insert
        ([ this coll pos & items]
          (->> (concat [coll pos] items)
              (apply cu-cl/insert)
              vec)))

      (replace
        ([ this coll from to & items]
          (cond (empty? items) coll
            :else
          (->> (concat [coll from to] items)
             (apply cu-cl/replace)
             vec)))

      #_(replace-1
        ([ this coll pos & items]
          (->>))) 
