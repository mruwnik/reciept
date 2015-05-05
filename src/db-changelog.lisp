(in-package #:reciept)

(defun create-db ()
  "initailises the database with the base tables"
  (postmodern:with-connection (db-params)
    (unless (postmodern:table-exists-p "users")
      (postmodern:execute (postmodern:dao-table-definition 'user)))
    (unless (postmodern:table-exists-p "currencies")
      (postmodern:query
       (:create-table currencies
		      ((id :type int4 :primary-key t)
		       (name :type varchar :unique t)))))
    (unless (postmodern:table-exists-p "reciepts")
      (postmodern:execute (postmodern:dao-table-definition 'reciept)))
    (unless (postmodern:table-exists-p "funds")
      (postmodern:query 
       (:create-table funds
		      ((id :type integer :primary-key t)
		       (userId :type integer :references (users :cascade :cascade))
		       (currency :type int4 :references (currencies :cascade :cascade)) 
		       (amount :type numeric :default 0)))))
    (unless (postmodern:table-exists-p "costs")
      (postmodern:with-connection (db-params)
	(postmodern:deftable (cost "costs")
	  (postmodern:!dao-def) ;; Import the existing info from the dao-class definition.
	  (postmodern:!foreign 'users 'userId 'id :on-delete :cascade :on-update :cascade)
	  (postmodern:!foreign 'currencies 'currency 'id :on-delete :cascade :on-update :cascade))
	(postmodern:create-table 'cost)))
    (unless (postmodern:table-exists-p "changelog")
      (postmodern:query
       (:create-table changelog
		      ((id :type varchar :primary-key t)
		       (operation :type varchar)
		       (time :type timestamp)
		       (status :type smallint)))))))

(defun clean-db ()
  (postmodern:with-connection (db-params)
    (postmodern:query (:drop-table :if-exists 'funds))
    (postmodern:query (:drop-table :if-exists 'costs))
    (postmodern:query (:drop-table :if-exists 'currencies))
    (postmodern:query (:drop-table :if-exists 'users))))


;;; make sure schema is correct
(defmacro modify-db (id operation)
  (let ((id-name (gensym)))
    `(let ((,id-name ,id))
       (unless (postmodern:query (:select '* :from 'changelog
			     :where (:and 
				     (:= 'id ,id-name)
				     (:= 'status 1))))
	 (handler-case 
	     (progn ,operation
		(postmodern:query 
		 (:insert-into 'changelog :set
			       'id ,id-name
			       'operation ,(princ-to-string operation)
			       'time (simple-date:universal-time-to-timestamp (get-universal-time))
			       'status 1)))
	   (postmodern:database-error (e) 
	     (print (concatenate 'string 
				 "db change: change "
				 ,id " caused error: "
				 (format NIL "~A" e)))))))))


(defun run-migrations()
  (postmodern:with-connection (db-params)
    (unless (postmodern:table-exists-p "users")
      (create-db))
    (unless (postmodern:table-exists-p "changelog")
      (postmodern:query
       (:create-table changelog
		      ((id :type varchar :primary-key t)
		       (operation :type varchar)
		       (time :type timestamp)
		       (status :type smallint)))))
    
    ;; db changes of the following format:
    ;;   (modify-db
    ;;     "<id>"
    ;;     (opertation))
    
    (modify-db
     "1307-20140310"
     (progn
       (postmodern:query 
	(:create-unique-index 'userNameIdx
			      :on users :fields name))
       (postmodern:query 
	(:create-index 'costsUserIdx
		       :on costs :fields userid reciept))))
    
   ; add a compilation table to hold user defined compilations 
    (modify-db
     "1151-20150423"
     (unless (postmodern:table-exists-p "compilations")
       (postmodern:execute (postmodern:dao-table-definition 'compilation))))))


