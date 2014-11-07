(defgeneric insert (table value &key db)
  (:documentation "insert the given value into the given table"))

(defgeneric select (table &key where order-by order-dir db)
  (:documentation "selects from the given table all values that fullfil the given where clause. the order-by and order-dir serve to specify in what order the data should be returned"))

(defgeneric update (rows values &key db)
  (:documentation "updates the given rows with the given valus"))

(defgeneric delete-from (from &optional where db)
  (:documentation "deletes the specified rows from the given table"))

;;; ######  ugly db stuff ###########
(defparameter *db* ())

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun create-backup(file &optional (timestamp (get-universal-time)))
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time timestamp)
    (save-db (format NIL "~a-~2,'0d-~2,'0d-~d" file date month year))))

(defmethod insert (table value &key (db *db*))
  (push value (getf db table)))

(let ((cost-id (reduce #'(lambda(a b)(max a (getf b :id)))
			     (getf *db* :costs)
			     :initial-value 0)))
	(defun next-cost() (incf cost-id)))

(if (not (use-real-database))
    (progn (load-db (get-costs-file))
	   (create-backup (get-costs-file)))
    (defparameter *db* (db-params)))

;; ### the 'where' clasue - right now it's only and - that should be fixed
(defmacro where-and(&rest clauses)
  (defun make-comparison-expr (field value)
    `(equal (getf row ,field) ,value))
  (defun make-comparisons-list (fields)
    (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))
  `#'(lambda (row) (and ,@(make-comparisons-list clauses))))

(defmacro where(clauses)
  `#'(lambda (row) ,clauses))

;; ###

(defmethod select (from &key (where NIL) (order-by NIL) 
			  (order-dir "desc") (db *db*))
  (let* ((rows (if where
		  (remove-if-not where (getf db from))
		  (getf db from)))
	 (comparator (cond 
		       ((numberp (getf (first rows) order-by)) 
			(if (equalp "asc" order-dir) #'< #'>))
		       ((stringp (getf (first rows) order-by)) 
			(if (equalp "asc" order-dir) #'string< #'string>))
		       (t #'(lambda (a b)t)))))
    (if order-by
	(sort (copy-seq rows) comparator
	      :key #'(lambda (row) (getf row order-by)))
	rows)))

(defmethod delete-from(from &optional where (db *db*))
  (setf (getf db from) 
	(if where
	    (remove-if where (getf db from))
	    ())))

(defmethod update(rows new-values &key (db *db*))
  (dolist (row rows)
    (do ((values new-values))
	((not values))
      (setf (getf row (pop values)) (pop values))))
  rows)

;;; ######  ugly db stuff end ###########


(defun import-db (&optional db-file)
  "imports data from a file db into a postgres database"
  (load-db (if db-file db-file (get-costs-file)))
  (postmodern:with-connection (db-params)
    ;; insert users
    (dolist (user (select :users))
      (postmodern:insert-dao (apply 'make-instance 'user user)))
    ;; insert currencies
    (let ((counter 0)
	  (currencies '()))
      (dolist (currency (remove-duplicates (mapcar #'(lambda (x) (getf x :currency)) (select :funds))))
	(postmodern:query 
	 (:insert-into 'currencies :set 'id (incf counter) 'name (princ-to-string currency)))
	(when (eq currency (get-default-currency-symbol))
	  (set-default-currency counter))
	(setf (getf currencies currency) counter))
      (setf (getf *db* :currencies) currencies))
    ;; insert funds
    (let ((currencies (getf *db* :currencies))
	  (counter 0))
      (dolist (fund (select :funds))
	(postmodern:query 
	 (:insert-into 'funds :set
		       'id (incf counter)
		     'userId (getf fund :userId)
		     'currency (getf currencies (getf fund :currency))
		     'amount (getf fund :amount)))))
    ;; insert costs
    (dolist (row (select :costs))
      (setf (getf row :timestamp) (simple-date:universal-time-to-timestamp (getf row :timestamp)))
      (setf (getf row :groups) (prin1-to-string (getf row :groups)))
      (setf (getf row :currency) (get-default-currency))
      (postmodern:insert-dao (apply 'make-instance 'cost row)))))
