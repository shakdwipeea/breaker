;;;; links.lisp

(in-package #:links)

;; Log headers to the REPL output stream
(setf drakma:*header-stream* *standard-output*)

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun get-request (url &key headers)
  "perform a get request get the stream and parse to json as plist"
  (jonathan:parse (dex:get url :headers headers) :as :plist))


(defun keywordize (value)
  (if (keywordp value) value (alexandria:make-keyword value)))


(defun get-in (plist &rest keys)
  (reduce #'(lambda (val key)
	      (getf val (keywordize key)))
	  keys
	  :initial-value plist))


(defun select-keys (plist &rest keys)
  (reduce (lambda (val key)
	    (append val (list (keywordize key) (getf plist (keywordize key)))))
	  keys
	  :initial-value '()))


(defconstant pl '(:abc "hello" :def "no"))

(select-keys pl :abc :def)

;;;;;;;;;;;;;;;;;
;; Application ;;
;;;;;;;;;;;;;;;;;

(defconstant user-agent '("User-Agent" "Mozilla/5.0 (Windows NT 6.1;) Gecko/20100101 Firefox/13.0.1"))


(defun parse-post-data (subreddit-json)
  (let ((children  (get-in subreddit-json "data" "children")))
    (mapcar (lambda (child)
	      (let ((data (get-in child "data")))
		(select-keys data
			     "url" "selftext" "ups" "downs" "author" "title" "created-utc")))
	    children)))


(defun fetch-subreddit-json (subreddit-name)
  (parse-post-data (get-request (concatenate 'string
					     "http://reddit.com/r/" subreddit-name ".json")
				:headers (list user-agent))))

(fetch-subreddit-json "clojure")

;; (assoc (intern "data" :keyword)  *res*)

;; (alexandria:hash-table)

;; (type-of  *res*)

;; (symbol-plist *res*)

;; (getf *res* )


