(ns haml-macro
  (:use haml-macro.core)
  (:use hiccup.core))

(defmacro haml [file]
  {:headers {"Content-Type" "text/html; charset=UTF-8"}
   :body (apply list 'hiccup.core/html (haml-macro.core/haml-file-with-layout file))})

(defn set-templates-dir [dir] (reset! *templates-dir* dir))