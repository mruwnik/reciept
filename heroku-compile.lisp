; -*- mode:common-lisp -*-

(print "loading reciept")
(ql:quickload :reciept) ;; load this, compiling if necessary.

(reciept:add-static-dir "/" "/app/public/")
(defparameter *server* nil)

;;; Initialize-application 
(defun initialize-application (&key port)
  ;; initialise the database
  (reciept:setup-database)
  ;; If we are restarting, say for example when we are developing,
  ;; then we need to stop any existing web server.
  (print "starting server")
  (when *server*
    (print "stopping server")
    (hunchentoot:stop *server*))

  ;; Start the web server.
  (setf *server*
	(reciept:start-server :port port :doc-root 
		(merge-pathnames "public/"
				 (asdf:system-source-directory :reciept)))))

