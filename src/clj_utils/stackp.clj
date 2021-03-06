(ns clj-utils.stackp
    (:refer-clojure :rename {pop core-pop
                             peek core-peek}
                    :exclude [remove replace])
    (:require [clj-utils 
                [core :as cu-c]
                [coll :as cu-cl]
                [core :as cu-cr]] ))

(defprotocol Stack+
"Stack+, a stack variant unifying functions working with indexed collections having the same concrete type. 

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
     
  (pop-n [this coll n] 
    "Removes the top (last) n elements, and yields 
     a tuple [values, newcoll] where values is a sub sequence of the removed values in 
     the order they were added (last index is first in the returned sub seq.), newcoll is 
     the resulting collection.")

  (remove [this coll] [this coll pos]
    "Removes the element at pos if provided, or the last element and yields the resulting collection.")

  (remove-n [this coll n]
    "Removes the last n elements from coll and yiels the resulting coll.")

  (remove-r [this coll from] [this coll from to]
    "Removes elements starting at from to the end of coll, or until 'to' 
     if provided.Yields the resulting collection.")

  (remove-rn [this coll pos] [this coll pos n] 
    "Removes n elements (or all remaining elements if n is not provided) 
     starting at pos and yields the resulting coll.")

  (remove-rnb [this coll pos n]
    "Removes n elements starting at pos and going backwards.
     Yields the resulting collection.")

  (peek [this coll] 
    "Yields the value of the last element.") 

  (peek-n [this coll n]
    "Yields a subcoll of the last n elements in their position order")

  (peek-nb [this coll n]
    "Yields a subcoll of the last n elements in the revese order of their position.")

  (peek-r [this coll from] [this coll from to]
    "Yields a subcoll of elements starting with from and ending at to, 
     or the remainder of coll if to is not provided.")

  (peek-rn [this coll pos n]
    "Yields a subcoll of n elements starting at pos.")

  (peek-rnb [this call pos n]
    "Yields a subcoll of n elements starting at pos going backwards, in reversed order.")

  (but [this coll] [this coll pos] 
    "Yields coll without the element at pos if provided, or the last otherwise.
     For implementations using immutable colls, this is equivalent to its counterpart remove signature.")

  (but-n [this coll n]
    "Yields a subcoll of all elements except the last n items.
     For implementations using immutable colls, this is equivalent to its counterpart remove signature.")

  (but-r [this coll from] [this coll from to]
    "Yields a subcoll without elements starting at from and up to 'to',
     or all remaining elements if to is not provided.
     For implementations using immutable colls, this is equivalent to its counterpart remove signature.")

  (but-rn [this coll pos n]
    "Yields a subcoll without the n elements starting at pos.
     For implementations using immutable colls, this is equivalent to its counterpart remove signature.")

  (but-rnb [this coll pos n]
    "Yields a subcoll without the n elements starting at pos, going backward.
     For implementations using immutable colls, this is equivalent to its counterpart remove signature.")

  (insert [this coll pos [items]] 
    "Unwraps and inserts items at pos and yields the resulting coll") 

  (insert-b [this coll pos [items]]
    "Unwraps and inserts items at (dec pos) in their reversed order of argument position, and yields the resulting coll") 

  (replace [this coll pos [items]] [this coll [items]]
    "Unwraps and replaces element at index pos (or after the last element if pos is not provided)
     in coll,  with items. Yields the resulting coll")

  (replace-b [this coll pos [items]]
    "Unwraps and replaces element at index pos in coll with items in their reversed arg order.")

  (replace-n [this coll n [items]]
    "Unwraps and replaces the last n elements of coll with items.")

  (replace-r [this coll from to [items]]
    "Unwraps and replaces coll elements indexed from start to (dec to) with items in order of their argument position, and yields the 
     resulting coll. If to is not provided all remaining elements after start pos. are replaced.")

  (replace-rn [this coll pos n [items]]
    "Unwraps and replaces n coll elements starting at pos with items.")

  (replace-rnb [this coll pos n [items]]
    "Unwraps and replaces n coll elements starting at pos going backwards, with items in their reversed arg order.")

  (push [this coll [items]]
    "Unwraps and pushes items on stack top; appends items to the end of coll"))


(defn vvec [ coll ]
    (if (vector? coll) coll (vec coll)))

(deftype DefaultStack+ []
;;"Stack+ default implementation, uses clojure vectors as input and does not keep state, i.e. a new vector is always
;; returned. If inputs are not vectors they are converted.
;;"
Stack+
      (pop [ this coll ] 
        (let [ v (vvec coll) ]
          [ (last v) (core-pop v) ]))
        
      (pop-n [ this coll n ] 
        (let [ siz (count coll) 
               start (- siz n)  
               popped (cu-cl/subsq coll start siz) ]
         [ (vvec popped)
             (-> (cu-cl/but-subsq coll start)
                vec)]))

      (remove 
         [ this coll ] 
          (-> (butlast coll) vec))
      (remove
         [ this coll pos ]
          (-> (cu-cl/but-subsq coll pos (inc pos)) vec))

      (remove-n 
         [ this coll n ]
           (let [ siz (count coll) ]
             (remove-r this coll (- siz n) siz))) 

      (remove-r
         [ this coll from ]
            (-> (cu-cl/but-subsq coll from) vec))
      (remove-r
         [ this coll from to ]
            (-> (cu-cl/but-subsq coll from to) vec))

      (remove-rn
         [ this coll pos n ]
            (-> (cu-cl/but-subsq coll pos (+ pos n)) vec))
      (remove-rn
         [ this coll pos]
           (-> (cu-cl/but-subsq coll pos) vec))

      (remove-rnb
         [ this coll pos n ]
            (let [start (-> pos (- n) inc)  end (inc pos)]
              (-> (cu-cl/but-subsq coll start end) vec)))

      (peek
        [ this coll ]
            (core-peek (vvec coll)))

      (peek-n
        [ this coll n ] 
          (let [ c (vvec coll) siz (count c) ]
                (subvec c (- siz n) siz)))

      (peek-nb
        [ this coll n ]
          (let [ siz (count coll) ]
            (-> (cu-cl/subsq coll (- siz n) siz) reverse vec)))


      (peek-r
        [ this coll from to ]
            (subvec (vvec coll) from to))
      (peek-r
        [ this coll from ]
           (let [ c (vvec coll) siz (count c) ]
             (peek-r this c from siz)))

      (peek-rn
        [ this coll from n ]
            (peek-r this coll from (+ from n)))

      (peek-rnb
        [ this coll from n ]
          (->
            (cu-cl/subsq coll (-> from (- n) inc) (inc from))
            reverse vec))

      (but
        [ this coll ]
          (remove this coll)
          #_(-> (butlast coll) vec))
      (but
        [ this coll pos ]
          (remove this coll pos)
          #_(-> (cu-cl/but-nth coll pos) vec))

      (but-n 
        [ this coll n ]
          (remove-n this coll n))

      (but-r
        [ this coll from to ]
          (remove-r this coll from to))
      (but-r
        [ this coll from ]
          (remove-r this coll from))

      (but-rn
        [ this coll from n ]
          (remove-rn this coll from n))

      (but-rnb 
        [ this coll from n ]
          (remove-rnb this coll from n))

      (insert
        [ this coll pos items ]
          (-> (apply cu-cl/insert coll pos items)
              vec))

      (insert-b
        [ this coll pos items ]
          (->> (reverse items) (apply cu-cl/insert coll pos) vec)) 
                            
      (replace
        [ this coll pos items ]
          (-> (apply cu-cl/replace-1 coll pos items) vec))
      (replace
        [ this coll items ]
          (replace this coll (count coll) items))

      (replace-b
        [ this coll pos items ]
          (->> (reverse items) (apply cu-cl/replace-1 coll pos) vec))

      (replace-n
        [ this coll n items ]
          (let [ siz (count coll) ]
            (-> (apply cu-cl/replace coll (- siz n) siz items) vec))) 
        
      (replace-r
        [ this coll from to items ]
          (-> (apply cu-cl/replace coll from to items)
             vec))
             
      (replace-rn 
        [ this coll pos n items ]
          (-> (apply cu-cl/replace coll pos (+ pos n) items) vec))

      (replace-rnb
        [ this coll pos n items ]
          (let [ siz (count coll) start (-> pos (- n) inc) end (inc pos) ]
             (->> (reverse items) (apply cu-cl/replace coll start end) vec)))

      (push
        [ this coll items ]
          (cond (empty? items) (vvec coll)
            :else
              (cu-c/thread-it (vvec coll) (apply conj it items)))))
