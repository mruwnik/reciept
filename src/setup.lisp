(in-package #:reciept)

(setf *random-state* (make-random-state t))

(defun getenv (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     #+CCL (ccl:getenv name)
     default))

(defun get-base-dir ()
  (concatenate 'string (getenv "HOME") "/"))

(defun get-document-root ()
  (merge-pathnames "public/" *default-pathname-defaults*))

(defun database-url () 
  (getenv "DATABASE_URL"))

(defparameter *local-db-params* (list "costs" "dan" "password" "localhost"))

(defun db-params ()
  "Heroku database url format is postgres://username:password@host/database_name. If we are testing on localhost, use the db-parameters from *local-db-params*."
  (if (database-url)
      (let* ((url (second (cl-ppcre:split "//" (database-url))))
	     (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@" url)))))
	     (host (first (cl-ppcre:split ":" (first (cl-ppcre:split "/" (second (cl-ppcre:split "@" url)))))))
	     (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@" url))))))
	(list database user password host))
      *local-db-params*))

