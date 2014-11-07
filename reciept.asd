;;;; reciept.asd

(asdf:defsystem #:reciept
  :serial t
  :description "a simple webapp for keeping track of ones costs"
  :depends-on  (#:cl-json
		#:cl-who
		#:parenscript
		#:ironclad
		#:hunchentoot
		#:postmodern
		#:simple-date
		#:zpng)
  :components ((:file "package")
	  	(:module :src
		 	:serial t
			:components (;(:file "main")
				     (:file "setup")
				     (:file "base")
				     (:file "classes")
				     (:file "database")
				     (:file "db-changelog")
				     (:file "utils")
				     (:file "web")
				     (:file "costs")
				     (:file "stats")
				     (:file "json")))))
;				     (:file "segment-graphics")
;				     (:file "ocr")))))
;))))
