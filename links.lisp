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


;;;;;;;;;;;;;;;;
;; CLOS utils ;;
;;;;;;;;;;;;;;;;


(defun get-slots (cls)
  "get slots of a class"
  (mapcar #'closer-mop:slot-definition-name
	  (closer-mop:class-direct-slots (class-of (make-instance cls)))))


(defun instance-from-plist (instance-type plist)
  "create a instance of class instance-type fetching data from plist"
  (apply #'make-instance (append (list instance-type)
				 (reduce (lambda (syms slot)
					   (append syms (list (keywordize slot)
							      (get-in plist slot))))
					 (get-slots instance-type)
					 :initial-value (list)))))


(defvar slots (get-slots 'post))


(defmacro enable-print (c)
  "add a generic print method to class c which prints all the slots of the class"
  (let* ((slots (get-slots c))
	 (fmt (reduce (lambda (str slot)
			  (concatenate 'string str " ~s " (symbol-name slot)))
			slots
			:initial-value "Value:")))
    `(defmethod print-object ((object post) stream)
       (print-unreadable-object (object stream :type t)
	 (with-slots ,slots object
	   (funcall #'format stream ,fmt ,@(mapcar (lambda (slot) (list 'print slot))  slots)))))))


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
	      :accessor subreddit)
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
	    :initarg :title
	    :accessor :title)
   (|created_utc| :col-type (:varchar 128)
		  :initarg :created_utc
		  :accessor created-utc))
  (:metaclass mito:dao-table-class))


(enable-print post)


(defun store-posts (posts)
  (mapcar #'mito:insert-dao posts))


;;;;;;;;;;;;;;;;;
;; Application ;;
;;;;;;;;;;;;;;;;;

(defun create-tables ()
  (mito:ensure-table-exists 'post))


(defvar user-agent '("User-Agent" "Mozilla/5.0 (Windows NT 6.1;) Gecko/20100101 Firefox/13.0.1"))


(defvar child-data '())

;; child-data is a particular post in the subreddit json
;; experments follow

;; (getf child-data :|created_utc|)

;; (created-utc (instance-from-plist 'post child-data))

;; (type-of (car (get-slots 'post)))

;; (get-in child-data :|title|)

(defvar cren '())

(defun parse-post-data (subreddit-json)
  (let ((children  (get-in subreddit-json "data" "children")))
    (defvar cren children)
    (format t "chidlren lengh ~d " (length children))
    (mapcar (lambda (child)
	      (instance-from-plist 'post (get-in child "data")))
	    children)))

;; (mapcar length child-data)


(defun fetch-subreddit-json (subreddit-name)
  (parse-post-data (get-request (concatenate 'string
					     "http://reddit.com/r/" subreddit-name ".json")
				:headers (list user-agent))))


(setq posts (fetch-subreddit-json "haskell"))

(created-utc (second posts))

(store-posts posts)



;; connect and perform migrations
(progn (connect)
       (create-tables))

;; (assoc (intern "data" :keyword)  *res*)

;; (alexandria:hash-table)

;; (type-of  *res*)

;; (symbol-plist *res*)

;; (getf *res* )


