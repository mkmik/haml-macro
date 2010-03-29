(ns haml-macro.core
  (:use [eu.dnetlib.clojure clarsec monad])
  (:require compojure))

(declare tag)

(defn not-nil? [p] (not (nil? p)))

(def newlinep (one-of "\n"))
(def sspace (one-of " "))

(defn prefixed [ch p]
  (>> (is-char ch) p))

(defn repeated [n p]
  (if (<= n 0)
	(result [])
	(m-sequence (repeat n p))))

(defn indented [level p] (let-bind [_ newlinep
									_ (repeated level sspace)]
								   p))

(def anyChar (not-char \newline))

(defn text [l] (>>== (many anyChar) #(apply str %)))
(defn textnl [l] (>>== (text l) #(str % "\n")))

(defn statement [l] (delay (either (tag l) (textnl l))))

(def tagPrefix (one-of "%#."))
(def tagChar (either letter digit (one-of "-_") tagPrefix))
(def tagName (let-bind [prefix tagPrefix
						rest   (many1 tagChar)]
					   (let [autoTag (if (not= \% prefix) "div")]
						 (result (keyword (apply str autoTag (if (not= \% prefix) prefix) rest))))))

(defn make-compojure-tag [t inline body]
  (apply vector (filter not-nil? (apply vector t inline body))))

(defn tag [l] 
  (let [nl (+ 2 l)]
	(let-bind [t      tagName
			   inline (optional (>> sspace (text 0)))
			   body   (optional (many1 (indented nl (statement nl))))]
			  (result (make-compojure-tag t inline body)))))


(defn statements [l] (followedBy (sepBy1 (statement l) newlinep) (optional newlinep)))

(def body (statements 0))

(def source
     (followedBy body (lexeme eof)))

(defn haml-str [strn]
  (:value (parse source strn)))

(defn haml-file [file]
  (haml-str (slurp file)))

(defn haml-file-html [file]
  (apply str (map compojure/html (haml-file file))))
