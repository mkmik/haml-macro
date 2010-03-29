(ns haml-macro.core
  (:use [eu.dnetlib.clojure clarsec monad])
  (:require compojure))

(declare tag)

;; general helpers

(defn not-nil? [p] (not (nil? p)))

;; parser helpers (should be in clarsec)

(defn >>= [pa pb]
  (let-bind [a pa
			 b pb]
			(result [a b])))

(defn not-one-of [target-strn]
  (let [str-chars (into #{} target-strn)]
    (satisfy #(not (contains? str-chars %)))))


(defn skipMany [p] (>>== (many p) (fn [x] nil)))

(defn repeated [n p]
  (if (<= n 0)
	(result [])
	(m-sequence (repeat n p))))


;; common tokens

(def newlinep (one-of "\n"))
(def sspace (one-of " "))


;; parser

(def anyChar (not-char \newline))

(def skipEmptyLine (followedBy (many sspace) newlinep))

(defn indented [level p] (let-bind [_ newlinep
									_ (many skipEmptyLine)
									_ (repeated level sspace)]
								   p))

(defn text [l] (>>== (many anyChar) #(apply str %)))
(defn textnl [l] (>>== (text l) #(apply str % "\n")))

(def expression (let-bind [_ (string "=")
						   code (many1 anyChar)]
						  (result (read (java.io.PushbackReader. (java.io.StringReader. (apply str code)))))))

(defn statement [l] (delay (either expression (tag l) (textnl l))))

(def tagPrefix (one-of "%#."))
(def tagChar (either letter digit (one-of "-_") tagPrefix))
(def tagName (let-bind [prefix tagPrefix
						rest   (many1 tagChar)]
					   (let [autoTag (if (not= \% prefix) "div")]
						 (result (keyword (apply str autoTag (if (not= \% prefix) prefix) rest))))))

(defn make-compojure-tag [t inline body]
  (apply vector (filter not-nil? (apply vector t inline body))))


(def inlineTag (let-bind [p (not-one-of " \n")
						  rest (text 0)]
						 (result (apply str p rest))))

(defn tagBody [l]
  (let [nl (+ 2 l)]
	(many1 (indented nl (statement nl)))))

(defn tag [l]
  (let-bind [t      tagName
			 _      (many sspace)
			 inline (optional inlineTag)
			 rest   (optional (tagBody l))]
			(result (make-compojure-tag t inline rest))))


(defn statements [l] (followedBy (sepBy1 (statement l) newlinep) (optional newlinep)))

(def body (statements 0))

(def source
     (followedBy body (lexeme eof)))

;;; parser end

;;; generators

(defn haml-str [strn]
  (:value (parse source strn)))

(def *templates-dir* (atom "views"))

(defn haml-file-name [file]
  (str @*templates-dir* "/" file ".haml"))

(defn haml-file [file]
  (haml-str (slurp (str @*templates-dir* "/" file ".haml"))))

(defn layout-path []
  (str "layouts/" "application"))

(defn build-layout [l]
  (list 'fn ['yield] (apply list 'list l)))

(defn load-layout []
  (let [lp (layout-path)]
	(build-layout (if (.exists (java.io.File. (str @*templates-dir* "/" lp ".haml")))
					(haml-file lp)
					'yield))))

(defn haml-file-with-layout [file]
  ((eval (load-layout)) (apply list 'list (haml-file file))))

(defn eval-haml-file [file]
  (eval (apply list compojure/html (haml-file file))))

