(ns clj-utils.io
  (:require [clojure.java.io :as io])
  (:import (java.io File)
           (org.apache.commons.io FileUtils)))


(defn file?
"Yields true if o is an instance of java.io.File; false otherwise.
"
[ o ] (= File (class o)))


(defn readstr
"Loads a text file's content into the returned string. The file path must be relative 
 to the classpath.
"
[ ^String filepath ]
    (FileUtils/readFileToString (File. filepath)))


(defn readlines
"Reads a text file's content into a seq of strings, one for each line"
[ ^String file-path ]
  (with-open [ rdr (io/reader file-path) ] (line-seq rdr)))


