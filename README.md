INTRODUCTION:

This is a Haml implementation for clojure. Currently the main target is the compojure framework.

Wherever haml expects ruby expressions, here you can use clojure expressions. In some cases ruby compatibility is maintained as much as possible,
especially to enable cut&paste of existing Haml templates from/to the ruby world.

USAGE:

    (ns example)
        (:use compojure)
        (:use haml-macro))

    (defroutes main-routes
      (GET "/hello/:first/:last" []
         (haml "index"))

    (def app
      (wrap-reload #'main-routes '(authfile.gui)))

    (run-jetty app {:port 4000}))


    (defroutes greeter
      (GET "/hello/:first/:last" (haml "example")))

see views/example.haml

you can change the directory where the templates are searched with:

    (set-template-dir "somepath")

haml is compiled to clojure code, you need to reload your module from REPL to see your changes in HAML.

you can put layouts in the views/layouts directory, by default it will be loaded a views/layouts/application.haml.
It should behave like the normal ruby haml layouts, using "yield":

    %html
        %body
            = yield
