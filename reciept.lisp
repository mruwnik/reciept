;;;; reciept.lisp

(ql:quickload "reciept")


(defparameter *server* 
  (reciept:start-server	:doc-root 
		(merge-pathnames "public/"
				 (asdf:system-source-directory :reciept))))

(ql:quickload "manifest") (manifest:start)
