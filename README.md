USAGE:

    (ns example)
        (:use compojure)
        (:use haml-macro))

    (defroutes greeter
      (GET "/hello/:first/:last" (haml "example.haml")))

see example.haml


