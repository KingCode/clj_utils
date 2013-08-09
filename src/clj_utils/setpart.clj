(ns clj-utils.setpart
  (:use [clj-utils.coll :only [in?]])
  (:use clojure.set) 
  (:gen-class))

(comment 
"Partitioning of collections by establishing transitive closures from grouping elements according
 to user provided binary predicates. 
 See ppartition function.
")

#_(defn make-set [ coll ] (reduce #(conj %1 %2) #{} coll))
(defn proper-subset? [ s1 s2 ] (let [ c1 (count s1) c2 (count s2) ] (and (< c1 c2) (subset? s1 s2))))
(defn supersets [ sets ] (for [ x sets y sets :when (proper-subset? x y) ] y))
(defn has-proper-superset? [ s1 s2 ] (not (empty? (filter #(proper-subset? s1 %) s2))))
(defn in-sets [ sets x ] (not (empty? (filter #(contains? % x) sets))))

(defn clos-step [ scoll ]
  (-> (for [x scoll y scoll :when (not (empty? (intersection x y)))] 
                                (union x y)) 
      (set)
      (difference scoll))) 

(defn merge-parts [ subs sups ]
    (let [ disj (-> (filter #(not (has-proper-superset? % sups)) subs)
                    (set)) ]
      (set (concat sups disj))))

(defn trans-clos
"Yields a set with contents of sets, which must be a coll of subsets. The resulting set
grows a superset for each transitive closure formed by subsets having common content.
"
[ sets ]
  (loop [ s sets ]
    (let [ subs (clos-step s) ]
      (if (empty? subs) s 
        (recur (merge-parts s subs))))))
                 

(defn ppartition 
"Partitions a coll based on a binary predicate between any two distinct elements of the collection.
When the predicate returns true the argument elements should belong to the same partition. 
Transitivity is assumed on a greedy basis, i.e. if  (pred a b) and (pred b c) are true the result 
is the same as if (pred a c) were true as well, whether or not (pred a c) is true. 
Example: 
         (ppartition  #(or (and (even? %1) (even? %2)) 
                               (and (odd? %1) (odd? %2))) [1 2 3 4 5])
yields the same contents as
        #{#{1 3 5} #{2 4}}
and similarly
        (ppartition #(= (%1 (dec %2))) [1 3 4 6 7 8]) 
with
        #{#{1} #{3 4} #{6 7 8}}
"
[ pred coll ]
    (let [ rels (for [ x coll y coll :when (and (not (= x y)) (pred x y))
                     ] #{x y}) 
           loners (-> (fn [x] (not (some #(in? % x) rels)))
                      (filter coll)) ]
       (->> (concat rels (map #(hash-set %) loners))
            (set)
            (trans-clos))))

