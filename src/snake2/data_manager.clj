(ns snake2.data-manager
  (:use clojure.java.io))

;Here '#^String is maybe type hinting'
(defn serialize [data-structure #^String filename]
  ;'with-open' ensures that the reader is closed at the end of the form
  (with-open [w (writer (file filename))]
    (print-dup data-structure w)))

(defn deserialize [filename]
  (with-open [r (java.io.PushbackReader. (reader filename))]
    (read r)))
