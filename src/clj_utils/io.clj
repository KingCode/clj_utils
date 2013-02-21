(ns clj-utils.io
  (:import (java.io File)
           (org.apache.commons.io FileUtils)))

(defn str-from-file
"Loads a text file's content into the returned string. The file path must be relative to the classpath"
[ filepath ]
    (FileUtils/readFileToString (File. filepath)))
