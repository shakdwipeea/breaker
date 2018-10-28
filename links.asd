;;;; links.asd

(asdf:defsystem #:links
  :description "Describe links here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:yason #:flexi-streams #:alexandria #:jonathan #:dexador #:mito)
  :components ((:file "package")
               (:file "links")))
