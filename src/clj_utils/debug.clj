(ns clj-utils.debug)

(defmacro func-name
"Extracts func's name.
"
[ func ]
    `(str (:name (meta (var ~func)))))


(defn around-advice
"An AOP's around-advice around f's execution. Yields the result of invoking (f args).
 Advice functions f-before and f-after are invoked with args and args and return value, resp.;
"
[ f f-before f-after & args ]
  (f-before args)
    (let [ rval (apply f args) ]
        (f-after args rval)
        rval))


(defmacro debug-info
"Prints to the console f's arguments and return values before and after execution, resp.
"
[ f & args ]
  `(let [ hdr# (str "FUNCTION '" (func-name ~f) "' -> ") ]
     (around-advice ~f
        #(println hdr# " ARGS: " %) 
        #(println hdr# " ARGS: "  %1 ", RETURN: " %2) ~@args)))


(defn debug-info-1
"Prints to the console f's arguments and return values before and after execution, resp.
"
[ f & args ]
  (let [ hdr (str "FUNCTION '" (str f) "' -> ") ]
    (apply around-advice f
        #(println hdr "ENTERING with:\n\tARGS: " %) 
        #(println hdr "EXITING:\n\tARGS: "  %1 "\n\tRETURN: " %2 "\n") args)))
