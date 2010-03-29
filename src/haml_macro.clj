(ns haml-macro
  (:use haml-macro.core))

(defmacro haml [file]
  (apply list compojure/html (haml-file file)))
