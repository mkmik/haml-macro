(ns haml-macro.core
  (:use [eu.dnetlib.clojure clarsec monad]))

(def newlinep (one-of "\n"))

(defn prefixed [ch p]
  (>> (is-char ch) p))

(def anyChar (not-char \newline))

(def tagPrefix (one-of "%#."))
(def tagChar (either letter digit (one-of "-_") tagPrefix))
(def tagName (let-bind [prefix tagPrefix
						rest   (many1 tagChar)]
					   (let [autoTag (if (not= \% prefix) "%div")]
						 (result (keyword (apply str autoTag prefix rest))))))

(def tag (let-bind [t tagName
					b (optional (>> space text))]
					(result [:tag t (second b)])))

(def text (>>== (many anyChar) #(vector :text (apply str %))))

(def statement (either tag text))

(def statements (followedBy (sepBy1 statement newlinep) (optional newlinep)))

(def body statements)

(def source
     (followedBy body (lexeme eof)))

(defn haml [strn]
  (:value (parse source strn)))
