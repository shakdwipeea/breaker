;;;; links.lisp

(in-package #:links)

;; Log headers to the REPL output stream
(setf drakma:*header-stream* *standard-output*)

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;

(defun get-request (url &key headers)
  "perform a get request get the stream and parse to json as plist"
  (jonathan:parse (dex:get url :headers headers)))


(defun keywordize (value)
  (if (keywordp value) value (alexandria:make-keyword value)))


(defun get-in (plist &rest keys)
  (reduce #'(lambda (val key)
	      (getf val (keywordize key)))
	  keys
	  :initial-value plist))


(defun select-keys (plist &rest keys)
  (reduce (lambda (val key)
	    (append val (cons (keywordize key) (getf plist (keywordize key)))))
	  keys
	  :initial-value '()))


(defconstant pl '(:abc "hello" :def "no"))

(select-keys pl :abc)

;;;;;;;;;;;;;;;;;
;; Application ;;
;;;;;;;;;;;;;;;;;

(defconstant req (get-request "http://reddit.com/r/clojure.json"
			      :headers '(("User-Agent" "Mozilla/5.0 (Windows NT 6.1;) Gecko/20100101 Firefox/13.0.1"))))



;; (assoc (intern "data" :keyword)  *res*)

;; (alexandria:hash-table)

;; (type-of  *res*)

;; (symbol-plist *res*)

;; (getf *res* )


