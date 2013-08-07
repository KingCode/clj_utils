(ns clj-utils.ra-stack
    (:refer-clojure :rename {pop core-pop})
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
  (pop [this coll]
    "Removes the top (last) element and returns a tuple of the removed element and the 
     resulting collection [ value, newcoll]." )
     
  #_(pop [this coll] 
    "Removes the top (last), or if provided, n elements, and yields 
     a tuple [values, newcoll] where values is a sub sequence of the removed values in 
     the order they were added (last index is first in the returned sub seq.), newcoll is 
     the resulting collection.")

  #_(remove [this coll] 
    "Removes the last element and yields the resulting collection.")

  #_(remove-r [this coll from] [this coll from to]
    "Removes elements starting at from to the end of coll, or until 'to' 
     if provided.Yields the resulting collection.")

  #_(remove-rn [this coll pos] [this coll pos n] 
    "Removes n elements (or all remaining elements if n is not provided) 
     starting at pos and yields the resulting coll.")

  #_(remove-rnb [this coll pos n]
    "Removes n elements starting at pos and going backwards.
     Yields the resulting collection.")

  #_(peek [this coll] 
    "Yields the value of the last element.") 

  #_(peek-n [this coll n]
    "Yields a subcoll of the last n elements in their position order")

  #_(peek-nb [this coll n]
    "Yields a subcoll of the last n elements in the revese order of their position.")

  #_(peek-r [this coll from] [this coll from to]
    "Yields a subcoll of elements starting with from and ending at to, 
    or the remainder of coll if to is not provided.")

  #_(peek-rn [this coll pos]
    "Yields a subcoll of n elements starting at pos.")

  #_(peek-rnb [this call pos]
    "Yields a subcoll of n elements starting at pos going backwards, in reversed order.")

  #_(but [this coll] [this coll pos] 
    "Yields coll without the element at pos if provided, or the last otherwise.")

  #_(but-n [this coll n]
    "Yields a subcoll of all elements except the last n items.")

  #_(but-r [this coll from] [this coll from to]
    "Yields a subcoll without elements starting at from and up to 'to',
     or all remaining elements if to is not provided.") 

  #_(but-rn [this coll pos]
    "Yields a subcoll without the n elements starting at pos.")

  #_(but-rnb [this coll pos]
    "Yields a subcoll without the n elements starting at pos, going backward.")

  #_(insert [this coll pos & items] 
    "Inserts items at pos and yields the resulting coll") 

  #_(insert-b [this coll pos & items]
    "Inserts items at pos in their reversed order of argument position, and yields the resulting coll") 

  #_(replace [this coll pos & items]
    "Replaces element at index pos in coll with items.")

  #_(replace-n [this coll & items]
    "Replaces the last n elements of coll with items.")

  #_(replace-r [this coll from to & items] 
    "Replaces coll elements indexed from start to (dec to) with items in order of their argument position, and yields the 
     resulting coll. If to is not provided all remaining elements after start pos. are replaced.")

  #_(replace-rn [this coll pos n & items]
    "Replaces n coll elements starting at pos with items.")

  #_(replace-rnb [this coll pos n & items ]
    "Replaces n coll elements starting at pos going backwards, with items.")

  #_(push [this coll & items] 
    "Pushes items on stack top; appends items to the end of coll"))


(def default-impl
"Stack+ default implementation, uses clojure vectors as input and does not keep state, i.e. a new vector is always
 returned. If inputs are not vectors they are converted.
"
  (let [ vvec #(if (vector? %) % (vec %)) ]
    (reify RAStack
      (pop 
        ([this coll] 
          #_(let [ v (vvec coll) ]
             [ (last v) (core-pop v) ]))) 
        
      #_(pop-n
         ([ this coll n ] 
             (->>
                (- (count coll) n)
                (cu-cl/but-subsq coll start)
                vec))
         ([ this coll ]
            (-> (butlast coll) vec)))

      #_(remove 
        ([ this coll ] 
          (butlast coll)))

      #_(remove-r
         ([ this coll from ]
            (-> (cu-cl/but-subsq coll from) vec))
         ([ this coll from to ]
            (-> (cu-cl/but-subsq coll from to) vec)))

      #_(remove-rn
         ([ this coll pos n ]
            (-> (cu-cl/but-subsq coll pos (+ pos n)) vec)))

      #_(remove-rnb
         ([ this coll pos n ]
            (let [start (- pos n) ]
              (-> (cu-cl/bus-subsq coll start pos) vec))))

      #_(peek-n
        ([ this coll n ] 
            (let [ c (vvec coll) siz (count c) ]
                (subvec c (- siz n) siz))))

      #_(peek-nb
        ([ this coll n ]
          (let [ siz (count c) ]
            (-> (cu-cl/subsq coll (- siz n) n) reverse vec))))

      #_(peek
        ([ this coll ]
            (clojure.core/peek (vvec coll))))

      #_(peek-r
        ([ this coll from to ]
            (subvec (vvec coll) from to))
        ([ this coll from ]
           (let [ c (vvec coll) siz (count c) ]
             (peek-r c from siz))))

      #_(peek-rn
        ([ this coll from n ]
            (peek-r coll from (+ from n)))) 

      #_(peek-rnb
        ([ this coll from n ]
            (peek-r coll (dec from n) from)))


      #_(but
        ([ this coll ]
          (butlast (vvec coll)))
        ([ this coll n ]
          (-> (cu-cl/but-nth coll n) vec)))

      #_(but-n 
        ([ this coll n ]
          (let [ c (vvec coll) (siz count c) ]
            (subvec c (- siz n) siz))))    

      #_(but-r
        ([ this coll from to ]
          (-> (cu-cl/but-subsq coll from to) vec))
        ([ this coll from ]
          (-> (cu-cl/but-subsq coll from) vec)))

      #_(but-rn
        ([ this coll from n ]
          (-> (cu-cl/but-subsq coll from (+ from n)) vec)))

      #_(but-rnb 
        ([ this coll from n ]
          (-> (cu-cl/but-subsq coll (- from n) from) vec)))

      #_(insert
        ([ this coll pos & items ]
          (->> (apply cu-cl/insert coll pos items)
              vec)))

      #_(replace-r
        ([ this coll from to & items ]
          (->> (apply cu-cl/replace coll from to items)
             vec)))

      #_(replace
        ([ this coll pos & items ]
          (->> (apply cu-cl/replace-1 coll pos items)))))))


