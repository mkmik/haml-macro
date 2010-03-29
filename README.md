USAGE:

    (ns example)
        (:use compojure)
        (:use haml-macro))

    (defroutes greeter
      (GET "/hello/:first/:last" (haml "example.haml")))

see example.haml



haml is compiled to clojure code, you need to reload your module from REPL to see your changes in HAML.

