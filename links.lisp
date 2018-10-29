;;;; links.lisp

(in-package #:links)

 
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


(defun get-slots (class)
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-direct-slots (class-of (make-instance class)))))


(defun instance-from-plist (instance-type plist)
  "create a instance of class instance-type fetching data from plist"
  (apply #'make-instance (append (list instance-type)
				 (reduce (lambda (syms slot)
					   (append syms (list (keywordize slot)
							      (get-in plist slot))))
					 (get-slots instance-type)
					 :initial-value (list)))))

;; (defconstant pl '(:abc "hello" :def "no"))

;; (select-keys pl :abc :def)


;;;;;;;;;;;;;;;;;
;; Persistence ;;
;;;;;;;;;;;;;;;;;

(defun connect ()
  (mito:connect-toplevel :sqlite3 :database-name "reddit"))


(defclass post ()
  ((|subreddit| :col-type (:varchar 64)
	      :initarg :subreddit
	      :accessor post-subreddit)
   (|url| :col-type (:varchar 256)
	:initarg :url)
   (|selftext| :col-type (:varchar 500)
	     :initarg :selftext)
   (|ups| :col-type :integer
	:initarg :ups)
   (|downs| :col-type :integer
	  :initarg :downs)
   (|author| :col-type (:varchar 64)
	   :initarg :author)
   (|title| :col-type (:varchar 256)
	  :initarg :title)
   (|created-utc| :col-type (:varchar 128)
		:initarg :created-utc))
  (:metaclass mito:dao-table-class))

;; (defun make-post (plist)
;;   "given a plist it will create a post by finding required elements from the plist"
;;   (apply make-instance
;; 	 (append (list 'post)
;; 		 )))

;;;;;;;;;;;;;;;;;
;; Application ;;
;;;;;;;;;;;;;;;;;

(post-subreddit (instance-from-plist 'post child-data))

(type-of (car (get-slots 'post)))

(get-in child-data :|title|)

(defvar user-agent '("User-Agent" "Mozilla/5.0 (Windows NT 6.1;) Gecko/20100101 Firefox/13.0.1"))


(defun parse-post-data (subreddit-json)
  (let ((children  (get-in subreddit-json "data" "children")))
    (mapcar (lambda (child)
	      (instance-from-plist 'post (get-in child "data")))
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


