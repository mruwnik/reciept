(in-package #:reciept)

(setf *random-state* (make-random-state t))

(defun getenv (target)
  #+ccl (ccl:getenv target)
  #+sbcl (sb-posix:getenv target))

(defun get-base-dir ()
  (concatenate 'string (getenv "HOME") "/"))

(defun get-costs-file ()
  (concatenate 'string (getenv "HOME") "/costs/costs"))

(defun get-document-root ()
  (merge-pathnames "public/" *default-pathname-defaults*))

(defun use-real-database () t)

(defparameter *database-url* (getenv "DATABASE_URL"))

;; ugly hack to let heroku know where the database is
(setf *database-url* "postgres://iitwacttphsvvf:c1urpCZ1bPXwlIHJwrnRZu1v0F@ec2-54-243-51-102.compute-1.amazonaws.com:5432/d7vgbt52t93ltc")
;;

(defparameter *local-db-params* (list "costs" "dan" "password" "localhost"))

(defun db-params ()
  "Heroku database url format is postgres://username:password@host/database_name. If we are testing on localhost, use the db-parameters from *local-db-params*."
  (if *database-url*
      (let* ((url (second (cl-ppcre:split "//" *database-url*)))
	     (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (host (first (cl-ppcre:split ":" (first (cl-ppcre:split "/" (second (cl-ppcre:split "@" url)))))))
	     (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@" url))))))
	(list database user password host))
      *local-db-params*))

;;; The home page of our tiny website.
(hunchentoot:define-easy-handler  (home-page :uri "/") ()
  "<html><body>
     <b>This is a web page!</b><br/>
     <img src='lisp-logo120x80.png'/>
   </body></html>")

(defvar *my-acceptor* nil)

;;; Initialize-application 
(defun initialize-application (&key port)

  ;; Set the dispatch table so easy-handler pages are served, 
  ;; and the files in <root>/static.
  (setf hunchentoot:*dispatch-table*
        `(hunchentoot:dispatch-easy-handlers
          ,(hunchentoot:create-folder-dispatcher-and-handler 
            "/" "/app/static/")))

  ;; If we are restarting, say for example when we are developing,
  ;; then we need to stop any existing web server.
  (when *my-acceptor*
    (hunchentoot:stop *my-acceptor*))

  ;; Start the web server.
  (setf *my-acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))

