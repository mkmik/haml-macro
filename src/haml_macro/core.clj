(ns haml-macro.core
  (:use [eu.dnetlib.clojure clarsec monad]))

(def newlinep (one-of "\n"))

(def anyChar (not-char \newline))

(defn prefixed [ch p]
  (>> (is-char ch) p))

(def tagName (prefixed \% baseIdentifier))
(def idName (prefixed \# baseIdentifier))
(def className (prefixed \. baseIdentifier))

(def tag (let-bind [t (optional tagName)
					i (optional idName)
					c (many className)
					b (optional (>> space text))]
				   (let [cc (if (empty? c) nil c)
						 tn (if (nil? t) "div" t)]
					 (if (every? nil? [t i cc])
					   fail
					   (result [:tag tn i c b])))))

(def text (>>== (many anyChar) #(vector :text (apply str %))))

(def statement (either tag text))

(def statements (followedBy (sepBy1 statement newlinep) (optional newlinep)))

(def body statements)

(def source
     (followedBy body (lexeme eof)))

(defn haml [strn]
  (:value (parse source strn)))
