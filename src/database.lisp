(in-package #:reciept)

(defparameter conversions '(:USD 102.41))

(defun add-reciept(desc user &key
			       (printed (get-universal-time))
			       (shop ""))
  (postmodern:with-connection (db-params)
    (postmodern:make-dao 'reciept :description desc
			 :userId user :shop shop 
			 :printed (simple-date:universal-time-to-timestamp printed)
			 :added (simple-date:universal-time-to-timestamp (get-universal-time)))))

(defun add-cost(desc amount user &key groups id
				   (currency :YEN)
				   (timestamp (get-universal-time))
				   reciept)
  (postmodern:with-connection (db-params)
    (postmodern:with-transaction () 
      (add-to-funds 
       (apply 'postmodern:make-dao 'cost 
	      (append (list
		       :userId user 
		       :description desc
		       :amount amount 
		       :timestamp (simple-date:universal-time-to-timestamp timestamp)
		       :groups (if groups 
				   (prin1-to-string groups)
					    "(:NONE)")
		       :currency (get-currency-id currency)
		       :reciept reciept)
		      (when id `(:id ,id))))))))

(defun edit-cost(id desc amount &key groups user reciept
				   currency timestamp)
  (postmodern:with-connection (db-params)
    (postmodern:with-transaction () 
      (let ((cost (postmodern:get-dao 'cost id)))
	(when cost
	  (when user
	    (setf (user cost) user))
	  (setf (description cost) desc)
	  (deduct-from-funds cost)
	  (setf (amount cost) amount)
	  (when reciept
	    (setf (reciept cost) reciept))
	  (when groups
	    (setf (groups cost) 
		  (prin1-to-string 
		   (if (and (= 1 (length groups))
			    (equal :NONE (first groups)))
		       groups 
		       (remove :NONE groups)))))
	  (when currency
	    (setf (currency cost) (get-currency-id currency)))
	  (when timestamp
	    (setf (timestamp cost) timestamp))
	  
	  (handler-case 
	      (progn
		(postmodern:update-dao cost)
		(add-to-funds cost)
		cost)
	    (cl-postgres:database-error (se) 
	      (postmodern:delete-dao cost)
	      (add-cost 
	       (description cost) (amount cost) (userid cost)
	       :groups (get-groups cost) :currency (currency cost)
	       :timestamp (simple-date:timestamp-to-universal-time
			   (timestamp cost))
	       :reciept (reciept cost)
	       :id (id cost)))))))))

(defun add-user (name password currency)
  "adds a new user. a salt is automaticly generated and used to hash the given password"
  (let ((salt (random-password 15)))
    (postmodern:with-connection (db-params)
      (postmodern:make-dao 
       'user :name name 
       :password (ironclad:byte-array-to-hex-string 
		  (ironclad:digest-sequence 
		   :sha256 (flexi-streams:string-to-octets 
			    (concatenate 'string salt password))))
       :salt salt
       :default-currency (get-currency-id currency)))))

(defmacro get-user-costs (userId &key (where NIL) 
				   (order-by :timestamp) 
				   (order-dir :desc))
  "returns all costs that satisfy the given where clause and which belong to the given user. this is a macro so as to get by postmoderns requirements - what that really means is that the where clause has to be static"
  `(postmodern:with-connection (db-params)
     (postmodern:select-dao 'cost 
			    ,(if where
				 `(:and (:= 'userId ,userId)
					,where)
				 `(:= 'userId ,userId))
			    ,(if (eq :desc order-dir)
				 `(:desc ,order-by)
				 order-by))))

(defmacro get-user-reciepts (userId &key (where NIL)
				      (order-by :printed) 
				      (order-dir :desc))
  "returns all reciepts that satisfy the given where clause and which belong to the given user. this is a macro so as to get by postmoderns requirements - what that really means is that the where clause has to be static"
  `(postmodern:with-connection (db-params)
     (postmodern:select-dao 'reciept
			    ,(if where
				 `(:and (:= 'userId ,userId)
					,where)
				 `(:= 'userId ,userId))
			    ,(if (eq :desc order-dir)
				 `(:desc ,order-by)
				 order-by))))

(defun fill-reciepts(userid reciepts &key (order-costs :description)
				       (order-costs-dir :desc))
  "fills the given reciepts with a list of their costs"
  (let* ((costs (get-user-costs 
		 userid :where
		 (:in 'reciept (:set (mapcar 'id reciepts)))
		 :order-by order-costs :order-dir order-costs-dir)))
    (dolist (reciept reciepts reciepts)
      (setf (costs reciept)
	    (remove-if-not 
	     #'(lambda (cost)(= (reciept cost) (id reciept))) costs)))))

(defun setup-currencies()
  "sets up a load of currency related functions and settings"
  (let ((default-currency 1)
	(default-currency-symbol :YEN)
	(currencies (let ((result '()))
		      (dolist (currency (postmodern:with-connection (db-params)
					  (postmodern:query (:select '* :from 'currencies))))
			(setf (getf result (make-keyword (second currency)))
			      (first currency)))
		      result)))
    
    (defun set-currencies (cur)
      (setf currencies cur))
    (defun set-default-currency (currency &optional symbol)
      (setf default-currency currency)
      (when symbol 
	(setf default-currency-symbol symbol)))
    (defun get-default-currency ()
      default-currency)
    (defun get-default-currency-symbol ()
      default-currency-symbol)
    (defun get-all-currencies ()
      currencies)

    (defgeneric get-user-default-currency (user)
      (:documentation "returns the default currency of this user"))
    (defmethod get-user-default-currency ((user user))
      (get-currency (default-currency user)))
    (defmethod get-user-default-currency ((user integer))
      (get-user-default-currency 
       (postmodern:with-connection (db-params)
	 (postmodern:get-dao 'user user))))
    
    (defgeneric get-currency-id (currency)
      (:documentation "returns the currency id for the given object"))
    (defmethod get-currency-id ((currency integer))
      currency)
    (defmethod get-currency-id ((currency symbol))
      (getf currencies currency))
    (defmethod get-currency-id ((currency cost))
      (get-currency-id (currency currency)))
    
    (defgeneric get-currency (cost)
      (:documentation "get the currency with the given id"))
    (defmethod get-currency ((cost integer))
      (getf (reverse currencies) cost))
    (defmethod get-currency ((currency symbol))
      currency)
    (defmethod get-currency ((cost cost))
      (if (numberp (currency cost))
	  (get-currency (currency cost))
	  (currency cost)))))

;; initialise the currency settings
(postmodern:with-connection (db-params)
  (when (postmodern:table-exists-p "currencies")
    (setup-currencies)))

(defgeneric delete-cost (userid cost)
  (:documentation "deletes the given cost and automaticly refreshes the amount of avaiable funds"))
(defmethod delete-cost (userid (cost cost))
  (when (= (userid cost) userid)
    (postmodern:with-connection (db-params)
      (postmodern:delete-dao cost))
    cost))
(defmethod delete-cost (userid (cost integer))
  (postmodern:with-connection (db-params)
    (delete-cost userid (postmodern:get-dao 'cost cost))))

(defgeneric delete-reciept (userid reciept)
  (:documentation "deletes the given reciept along with all atached costs."))
(defmethod delete-reciept (userid (reciept reciept))
  (when (= (userid reciept) userid)
    (postmodern:with-connection (db-params)
      (dolist (cost (postmodern:select-dao 'cost (:= 'reciept (id reciept))))
	(deduct-from-funds (id cost))
	(delete-cost userid cost))
      (postmodern:delete-dao reciept))
    (apply 'add-to-undo-list userid (apply 'list reciept (costs reciept)))
    reciept))
(defmethod delete-reciept (userid (reciept integer))
  (postmodern:with-connection (db-params)
    (delete-reciept userid 
		    (first (fill-reciepts 
			    userid 
			    (list (postmodern:get-dao 'reciept reciept)))))))

;;; funds utils
(defgeneric get-funds (user &optional currency)
  (:documentation "gets a list of all of the given users funds"))
(defmethod get-funds ((user user) &optional currency)
  (postmodern:with-connection (db-params)
    (let ((result '()))
      (dolist (fund (postmodern:query 
		     (:select 'currency 'amount :from 'funds
			      :where (:= 'userId (id user))))
	       result)
	(unless (= 0 (second fund))
	  (push `(:currency ,(get-currency (first fund))
			    :amount ,(second fund)) result)))
      (if (not currency)
	  result 
	  (dolist (fund result)
	    (when (equal (getf fund :currency) (get-currency currency))
	      (return fund)))))))

(defmethod get-funds ((user integer) &optional currency)
  (postmodern:with-connection (db-params)
    (get-funds (postmodern:get-dao 'user user) currency)))

(defgeneric modify-funds (cost operation)
  (:documentation "modifies the users funds on the basis of the given cost"))
(defmethod modify-funds ((cost cost) operation)
  (postmodern:with-connection (db-params)
    (when (eq :- operation)
      (setf (amount cost) (- (amount cost))))
    (let ((result (postmodern:query (:update 'funds 
			       :set 'amount (:+ 'amount (amount cost))
			       :where (:and (:= 'userId (userId cost))
					    (:= 'currency (get-currency-id cost)))
			       :returning 'amount))))
      (when (eq :- operation)
	(setf (amount cost) (- (amount cost))))
      (unless result
	  (postmodern:query 
	   (:insert-into 'funds :set 
			 'id (:+ 1 (:select (:max 'id) :from 'funds))
			 'userId (userId cost) 
			 'amount (amount cost) 
			 'currency (get-currency-id cost))))))
  cost)

(defmethod modify-funds ((cost integer) operation)
  (postmodern:with-connection (db-params)
    (modify-funds (postmodern:get-dao 'cost cost) operation)))

(defun add-to-funds (cost)
  (modify-funds cost :+))

(defun deduct-from-funds (cost)
  (modify-funds cost :-))


;; undo-map is a map of the following format
;;  {userid: ((timestamp . (operations))
;;            (timestamp . (operations))
;;             ...)}

(defparameter *user-map* (make-hash-table))

(defgeneric restore-object (userid obj)
  (:documentation "restores the given deleted object, but first checks whether the object belongs to the given user"))
(defmethod restore-object (userid (reciept reciept))
  (when (= (userid reciept) userid)
    (postmodern:with-connection (db-params)
      (postmodern:insert-dao reciept))
    reciept))
(defmethod restore-object (userid (cost cost))
  (when (= (userid cost) userid)
    (postmodern:with-connection (db-params)
      (postmodern:insert-dao cost))
    (add-to-funds cost)
    cost))
(defmethod restore-object (userid (obj integer))
  obj)

(defun add-to-undo-list(userid &rest objs)
  (setf (gethash userid *user-map*)
	(cons (cons (get-universal-time) objs)
	      (gethash userid *user-map*))))

(defun undo-last-operation(userid)
  (dolist (obj (rest (first (gethash userid *user-map*))) 
	   (pop (gethash userid *user-map*)))
    (restore-object userid obj)))

(defun avaiable-undos(userid)
  (length (gethash userid *user-map*)))
