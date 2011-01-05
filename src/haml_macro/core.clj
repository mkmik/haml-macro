(ns haml-macro.core
  (:use [eu.dnetlib.clojure clarsec monad]))

(declare tag clojure-statement)

                                        ; General Helpers

(defn not-nil? [p]
  (not (nil? p)))

                                        ; Parser Helpers (*should be in clarsec*)

(defn >>= [pa pb]
  (let-bind [a pa
             b pb]
            (result [a b])))

(defn not-one-of [target-strn]
  (let [str-chars (into #{} target-strn)]
    (satisfy #(not (contains? str-chars %)))))


(defn skip-many [p]
  (>>== (many p) (fn [x] nil)))

(defn repeated [n p]
  (if (<= n 0)
    (result [])
    (m-sequence (repeat n p))))


                                        ; Common Tokens
(def new-line (one-of "\n"))
(def single-space (one-of " "))

;; TODO: Fix this: accept colons
(def xml-tag-name baseIdentifier)


                                        ; Parser
(def any-char (not-char \newline))

(def skip-empty-line (followedBy (many single-space) new-line))

(defn indented [level p]
  (let-bind [_ new-line
             _ (many skip-empty-line)
             _ (repeated level single-space)]
            p))

(defn text [l]
  (>>== (many any-char) #(apply str %)))

(defn text-newline [l]
  (>>== (text l) #(apply str % "\n")))

(def expression
     (let-bind [_ (string "=")
                code (many1 any-char)]
               (result (read (java.io.PushbackReader. (java.io.StringReader. (apply str code)))))))

(defn statement [l] (delay (either expression (clojure-statement l) (tag l) (text-newline l))))

(def tag-prefix (one-of "%#."))

(def tag-char (either letter digit (one-of "-_") tag-prefix))

(def tag-name (let-bind [prefix tag-prefix
                        rest   (many1 tag-char)]
                       (let [autoTag (if (not= \% prefix) "div")]
                         (result (keyword (apply str autoTag (if (not= \% prefix) prefix) rest))))))

(defn make-compojure-tag [t attrs inline body]
  (apply vector (filter not-nil? (apply vector t attrs inline body))))


(defn not-char-of [s]
  (satisfy #(not (contains? s %))))

;; hack, probably incorrect but allow me to handle the '#{}' thing
(defn not-followed-by [p trailing]
  (make-monad 'Parser
              (fn p-try-parse [strn]
                (let [res (parse p strn)]
                  (if (consumed? res)
                    (let [trail (parse trailing (:rest res))]
                      (if (consumed? trail)
                        (failed)
                        res))
                    (failed))))))


(defn quoted-string [ch]
  (let [quoteSeparator (is-char ch)
        stringBody (stringify (many1 (either (not-char-of #{ch \# \newline}) (not-followed-by (string "#") (string "{")))))
        expressionBody (let-bind [_ (string "#{")
                                  expr (stringify (many1 (not-char \})))
                                  _ (string "}")]
                                 (result (read-string expr)))
        expansionBody (many (either stringBody expressionBody))
        optimize (fn [exp] (if (== 2 (count exp)) (second exp) exp))]
    (>>== (between quoteSeparator quoteSeparator expansionBody) #(optimize (apply list 'str %)))))

(def haml-string-literal
     (lexeme (either (quoted-string \') (quoted-string \"))))

(def ruby-attr-pair
     (let-bind [name (lexeme (either haml-string-literal (>> (string ":") baseIdentifier)))
                _    (lexeme (string "=>"))
                value haml-string-literal]
               (result {(keyword name) value})))

(def ruby-attr-list
     (>>== (braces (sepBy ruby-attr-pair comma))
           #(apply merge %)))

(def html-attr-pair
     (let-bind [name (lexeme xml-tag-name)
                _    (lexeme (string "="))
                value haml-string-literal]
               (result {(keyword name) value})))

(def html-attr-list
     (>>== (parens (sepBy html-attr-pair single-space))
           #(apply merge %)))

(def attr-list (either ruby-attr-list html-attr-list))

(def inline-tag
     (let-bind [p (not-one-of " \n")
                rest (text 0)]
               (result (apply str p rest))))

(defn tag-body [l]
  (let [nl (+ 2 l)]
    (many1 (indented nl (statement nl)))))

(defn tag [l]
  (let-bind [t      tag-name
             attrs  (optional attr-list)
             _      (many single-space)
             inline (optional inline-tag)
             rest   (optional (tag-body l))]
            (result (make-compojure-tag t attrs inline rest))))

(defn clojure-statement [l]
  (let-bind [_      (string "-")
             _      (many single-space)
             code (many1 any-char)
             rest   (optional (tag-body l))]
            (result (concat (read (java.io.PushbackReader. (java.io.StringReader. (apply str code)))) rest))))


(defn statements [l] (followedBy (sepBy1 (statement l) new-line) (optional new-line)))

(def body (statements 0))

(def source
     (followedBy body (lexeme eof)))
                                        ; Parser End


                                        ; Generators
(defn haml-str [strn]
  (:value (parse source strn)))

(def *templates-dir* (atom "views"))

(defn haml-file-name [file]
  (str @*templates-dir* "/" file ".haml"))

(defn haml-file [file]
  (haml-str (slurp (str @*templates-dir* "/" file ".haml"))))

(defn layout-path []
  (str "layouts/" "application"))

(def *layout-path*
     "layouts/application")

(defn build-layout [l]
  ;; (list 'fn ['yield] (apply list 'list l)))
  (apply list 'list l))

(defn load-layout []
  (let [lp *layout-path*]
    (build-layout
     (if (.exists (java.io.File. (str @*templates-dir* "/" lp ".haml")))
       (haml-file lp)
       '(yield)))))

(defn haml-file-with-layout [file]
  (let [loaded-layout (load-layout)
        body (apply list 'list (haml-file file))]
    [(list 'let ['yield body] loaded-layout)]))
