(ns haml-macro
  (:use haml-macro.core))

(defmacro haml [file]
  (apply list compojure/html (haml-macro.core/haml-file-with-layout file)))

(defn set-templates-dir [dir] (reset! *templates-dir* dir))