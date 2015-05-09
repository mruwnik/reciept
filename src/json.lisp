(in-package #:reciept)

(defun json-data-handler (&optional id)
  (with-auth (username userid)
    (setf (hunchentoot:content-type*) "text/javascript")
    (cond
      ((equal id "costs") (get-costs :userid userid :limit 2))
      ((equal id "reciepts") 
       (get-reciepts :userid userid 
		     :sort-by (make-symbol (get-default (hunchentoot:get-parameter "sort-by") "printed"))
		     :sort-dir (get-default (hunchentoot:get-parameter "sort-dir") "desc")
		     :limit (get-default (hunchentoot:get-parameter "limit") 15)
		     :offset (get-default (hunchentoot:get-parameter "offset") 0)))
      (t ""))))

(setf hunchentoot:*dispatch-table*
	(append hunchentoot:*dispatch-table* 
		(list (create-regex-dispatcher
		       "^/data/(.*?)\.json$" 'json-data-handler))))

(defun timestamp-to-json-timestamp(timestamp)
  ; simple-date counts time from 1900, not 1970, so all timestamps
  ; will be 25567 days older than they should be, while angular has
  ; milisecond timestamps - hence the magic numbers
  (* (- (simple-date:timestamp-to-universal-time timestamp)
	(* 25567 24 60 60)) 1000))
(defgeneric to-json (object)
  (:documentation "returns the given object in a json representation"))
(defmethod to-json ((objs list))
  (mapcar #'to-json objs))
(defmethod to-json ((cost cost))
  `(("id" . ,(id cost))
    ("description" . ,(description cost))
    ("amount" . ,(amount cost))
    ("currency" . ,(get-currency cost))
    ("timestamp" . ,(timestamp-to-json-timestamp (timestamp cost)))
    ("groups" . ,(loop for group in (get-groups cost)
		     collecting (cons group t)))
    ("groupslist" . ,(get-groups cost))))
(defmethod to-json ((reciept reciept))
  `(("id" . ,(id reciept))
    ("description" . ,(description reciept))
    ("shop" . ,(shop reciept))
    ("printed" . ,(timestamp-to-json-timestamp (printed reciept)))
    ("added" . ,(timestamp-to-json-timestamp (added reciept)))
    ("costs" . ,(loop for cost in (costs reciept) 
		   collecting (to-json cost)))
    ("amount" . ,(reduce '+ (mapcar 'amount (costs reciept))))
    ("currency" . ,(when (costs reciept) 
			 (get-currency (first (costs reciept)))))))

(hunchentoot:define-easy-handler (reciepts-controller
				  :uri (get-page-address :reciepts-controller))
    ()
  (with-auth (username userid)
    (setf (hunchentoot:content-type*) "text/javascript")
    (concatenate 'string
      (ps
	(defvar reciept-App (chain angular (module "recieptApp" 
						   (array "recieptControllers" "infinite-scroll"))))
	
	(defvar reciept-controllers 
	  (chain angular (module "recieptControllers" (array)))))
      (format NIL "~%~{~a~%~}" 
	      (loop for func in 
		   '(add-reciept-ctrl tabs-ctrl
		     costs-list-ctrl infinite-reciepts-ctrl
		     handle-reciepts save-cost-json
		     lisp-functions) collecting 
		   (funcall func username userid)))
	)))

(defun infinite-reciepts-ctrl (username userid)
  (ps
    (chain reciept-controllers
	   (controller
	    "RecieptsListCtrl"
	    (lambda ($scope |Reciepts|)
	      (setf (chain $scope reciepts) (new (|Reciepts|)))
	      (setf (chain $scope groups)
		    (lisp (append `(return-lisp-list)
				  (remove :NONE 
					  (get-all-groups userid)))))
	      (setf (chain $scope save-cost)
		    (lambda (id)
		      (setf selected (chain $scope reciepts selected))
		      (setf cost
			    (if (=== id undefined) 
				(chain selected new-cost)
				(getprop (chain selected costs) id)))
		      (chain 
		       (save-cost cost (chain selected id))
		       (success
			(lambda(data)
			  (chain 
			   $scope 
			   ($apply
			    (lambda ()
			      (when (=== id undefined)
				(unless (getprop selected "costs")
				  (setf (getprop selected "costs") (array)))
				(setf (chain selected costs)
				      (append (chain selected costs) 
					      (chain data 0 costs 0)))
				(delete (chain selected new-cost)))
			      (setf (chain selected amount) 0)
			      (dolist (c (chain selected costs))
				(incf (chain selected amount)
				      (parse-float (chain c amount))))
			      (if (getprop cost "edit")
				  (setf (getprop cost "edit") NIL)
				  (setf (getprop cost "edit") t)))))))))))))))

(defun handle-reciepts (username userid)
  (ps
    (chain reciept-Controllers
	   (factory "Reciepts" 
		    (lambda($http)
		      (setf |Reciepts| 
			    (lambda ()
			      (setf (chain this items) '())
			      (setf (chain this selected) NIL)
			      (setf (chain this busy) false)
			      (setf (chain this after) "")
			      (setf (chain this sort-by) NIL)
			      (setf (chain this sort-dir) "desc")))
		      (setf (chain |Reciepts| prototype next-page)
			    (lambda ()
			      (when (chain this busy) return)
			      (setf (chain this busy) true)
			      (setf url (lisp (get-page-address :reciepts-json)))
			      (setf offset (if (chain this items)
					       (chain this items length)
					       0))
			      (chain $http 
				     (get url 
					  (create :params 
		  			     (create :offset offset
						     :sort-by (chain this sort-by)
						     :sort-dir (chain this sort-dir))))
				     (success 
				      (chain 
				       (lambda (data)
					 (setf (chain this items)
					       (append (chain this items) data))
					 (setf 
					  (chain this after)
					  (+ "t3_" 
					     (chain
					      (getprop
					       (chain this items)
					       (1- (chain this items length)))
					      id)))
					 (setf (chain this busy) false))
				       (bind this)))
				     (error (lambda (data) 
					      (chain console (log data)))))))
		      (setf (chain |Reciepts| prototype toggle)
			    (lambda(id)
			      (setf (chain this selected)
				    (getprop (chain this items) id))
			      (if (getprop 
				   (getprop (chain this items) id)
				   "selected")
				  (setf (getprop 
					 (getprop (chain this items) id)
					 "selected") NIL)
				  (setf (getprop 
					 (getprop (chain this items) id) "selected") t))))
		      (setf (chain |Reciepts| prototype sort-column)
			    (lambda (column dir)
			      (setf (chain this sort-dir)
				    (if dir dir
					(if
					 (and (equal (chain this sort-by) column)
					      (equal (chain this sort-dir) "desc"))
					   "asc" "desc")))
			      (setf (chain this sort-by) column)
			      (setf (chain this items) (array))
			      (setf (chain this selected) NIL)
			      ((chain this next-page))))
		      (setf (chain |Reciepts| prototype toggle-edit-cost)
			    (lambda(id)
			      (setf cost 
				    (if (=== id undefined)
					(chain this selected new-cost)
					(getprop (chain this selected costs) id)))
			      (unless cost
				(setf (chain this selected new-cost) 
				      (create :groups (create)
					      :groupslist (array)))
				(setf cost (chain this selected new-cost)))
			      (setf (getprop cost "edit")
				    (if (getprop cost "edit") NIL t))))
		      |Reciepts|)))))

(defun add-reciept-ctrl (username userid)
    (ps
      (chain reciept-controllers 
	     (controller
	      "AddRecieptCtrl" 
	      (array "$scope"
		     (lambda ($scope)
		       (setf (chain $scope date) (new (|Date|)))
		       (setf (chain $scope groups)
			     (lisp (append `(return-lisp-list)
					   (reverse (remove :NONE (get-all-groups userid))))))
		       (setf (chain $scope total)
			     (lambda (costs)
			       (setf total 0)
			       (chain angular 
				      (|forEach| costs
					       (lambda (cost) 
						 (incf total 
						       (|parseFloat| (chain cost amount))))))
			       total))
		       (setf (chain $scope add-cost)
			     (lambda() 
			       (defvar groups
				 (remove-if-not 
					  (lambda(p)(getprop 
						     (chain $scope cost groups) p))
					  (chain |Object| 
						 (keys 
						  (chain $scope cost groups)))))
			       (when (chain $scope cost new-groups)
				 (setf groups (append groups
						      (chain $scope cost new-groups))))
			       (setf (chain $scope new-costs)
				     (append (chain $scope new-costs)
				       (create
					:id (incf ids)
					:amount (chain $scope cost amount)
					:description (chain $scope cost description)
					:groups groups
					:groupsString (chain groups (join ", ")))))
			       (delete (chain $scope cost amount))
			       (delete (chain $scope cost description))))
		       
		       (setf (chain $scope remove-new-cost)
			     (lambda(id)
			       (chain $scope new-costs (splice id 1))))
			     
		       (defvar ids 1)
		       (setf (chain $scope new-costs) (array))
		       (setf (chain $scope cost) 
			     (create :groups (create)))))))))

(defun tabs-ctrl (username userid)
  (ps
    (chain reciept-controllers 
	   (controller
	    "TabsCtrl" 
	    (array "$scope"
		   (lambda ($scope)
		     (setf (chain  $scope tabs)
			   (array (create "selected" NIL)
				  (create "selected" t)
				  (create "selected" NIL)
				  (create "selected" NIL)))
		     (setf (chain $scope show-tab)
			   (lambda (id)
			     (dolist (tab (chain $scope tabs))
			       (setf (getprop tab "selected") NIL))
			     (setf (getprop (getprop (chain $scope tabs) id) "selected") t)))))))))

(defun costs-list-ctrl (username userid)
  (ps
    (chain reciept-controllers 
	   (controller
	    "CostsListCtrl" 
	    (array "$scope" "$http"
		   (lambda ($scope $http)
		     (chain $http 
			    (get (lisp (get-page-address :costs-json)))
			    (success (lambda(data)
				       (setf (chain $scope costs)
					     data))))))))))

(defun save-cost-json (username userid)
  (ps
    (defun save-cost (cost reciept-id)
      (setf groups '())
      (for-in (k (@ cost groups))
	      (when (getprop (chain cost groups) k)
		(chain groups (push k))))
      (setf data
	    (create
	     :ajax T
	     :reciept-id reciept-id
	     :amount (chain cost amount)
	     :description (@ cost description)
	     :groups groups
	     :new-groups (@ cost newGroups)))
      (when (in "id" cost)
	(setf (getprop data "id") (chain cost id)))
      (chain $ 
	     (post (lisp (get-page-address :add-cost)) data)))))

(defun lisp-functions (username userid)
  (ps
    (defun remove-if-not (pred objs)
      (defvar result (array))
      (dolist (obj objs result)
	(when (pred obj)
	  (setf result (append result obj)))))

    (defun remove-if (pred objs)
      (defvar result (array))
	(dolist (obj objs result)
	  (unless (pred obj)
	    (setf result (append result obj)))))
    
    (defun mapcar(f objs)
      (defvar result (array))
      (dolist (obj objs result)
	(setf result (append result (f obj)))))
    
    (defun get-value(obj)
      (lambda(field)
	(getprop obj field)))
    
    (defun return-lisp-list() arguments)))
