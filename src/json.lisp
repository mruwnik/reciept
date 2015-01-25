(in-package #:reciept)

(defun json-data-handler (&optional id)
  (with-auth (username userid)
    (setf (hunchentoot:content-type*) "text/javascript")
    (cond
      ((equal id "costs") (get-costs :userid userid))
      ((equal id "reciepts") (get-reciepts :userid userid))
      (t ""))))

(setf hunchentoot:*dispatch-table*
	(append hunchentoot:*dispatch-table* 
		(list (create-regex-dispatcher
		       "^/data/(.*?)\.json$" 'json-data-handler))))


(defun timestamp-to-json-timestamp(timestamp)
  (* (simple-date:timestamp-to-universal-time timestamp) 1000))
(defgeneric to-json (object)
  (:documentation "returns the given object in a json representation"))
(defmethod to-json ((cost cost))
  `(("id" . ,(id cost))
    ("description" . ,(description cost))
    ("amount" . ,(amount cost))
    ("currency" . ,(get-currency cost))
    ("timestamp" . ,(timestamp-to-json-timestamp (timestamp cost)))
    ("groups" . ,(loop for group in 
		       (get-groups (first (get-user-costs 1)))
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
    (ps
      (defvar reciept-App (chain angular (module "recieptApp" 
					       (array "recieptControllers"))))
      
      (defvar reciept-controllers 
	    (chain angular (module "recieptControllers" (array))))
      (chain reciept-controllers 
	     (controller
	      "AddRecieptCtrl" 
	      (array "$scope"
		     (lambda ($scope)
		       (setf (chain $scope groups)
			     (lisp (append `(return-lisp-list)
					   (remove :NONE (get-all-groups userid)))))
		       
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
			       (setf (chain $scope cost) 
				     (create :groups (create)))))
		       
		       (setf (chain $scope remove-new-cost)
			     (lambda(id)
			       (chain $scope new-costs (splice id 1))))
			     
		       (defvar ids 1)
		       (setf (chain $scope new-costs) (array))
		       (setf (chain $scope cost) 
			     (create :groups (create)))))))
      
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
			       (setf (getprop (getprop (chain $scope tabs) id) "selected") t)))
		       
		       ))))

      (chain reciept-controllers 
	     (controller
	      "CostsListCtrl" 
	      (array "$scope" "$http"
		     (lambda ($scope $http)
		       (chain $http 
			      (get (lisp (get-page-address :costs-json)))
			      (success (lambda(data)
					 (setf (chain $scope costs)
					       data))))))))
      (chain reciept-controllers 
	     (controller 
	      "RecieptsListCtrl" 
	      (array "$scope" "$http"
		     (lambda ($scope $http)
		       (chain $http 
			      (get (lisp (get-page-address :reciepts-json)))
			      (success (lambda(data)
					 (setf (chain $scope reciepts)
					       data))))
		       (defvar selected null)
		       (setf (chain $scope toggle-reciept)
			     (lambda(id)
			       (setf selected
				     (getprop (chain $scope reciepts) id))
			       (if (getprop 
				    (getprop (chain $scope reciepts) id)
				    "selected")
				   (setf (getprop 
				    (getprop (chain $scope reciepts) id)
				    "selected") NIL)
				   (setf (getprop 
					  (getprop (chain $scope reciepts) id) "selected") t))))
		       (setf (chain $scope groups)
			     (lisp (append `(return-lisp-list)
					   (remove :NONE (get-all-groups userid)))))
		       (setf (chain $scope toggle-edit-cost)
			     (lambda(id)
			       (if (getprop 
				    (getprop (chain selected costs) id)
				    "edit")
				   (setf (getprop 
				    (getprop (chain selected costs) id)
				    "edit") NIL)
				   (setf (getprop 
					  (getprop (chain selected costs) id) "edit") t))))
		       (setf (chain $scope save-cost)
			     (lambda(id)
			       (setf cost (getprop (chain selected costs) id))
			       (setf groups '())
			       (for-in (k (@ cost groups))
				 (when (getprop (chain cost groups) k)
				   (chain groups (push k))))
				(chain $ 
				       (post
					(lisp (get-page-address :add-cost))
					(create
					 :ajax T
					 :id (chain cost id)
					 :reciept-id (chain selected id)
					 :amount (chain cost amount)
					 :description (@ cost description)
					 :groups groups
					 :new-groups (@ cost newGroups))
				       (lambda(data)
					 (chain $scope ($apply
							(lambda()
							(setf 
							 (chain selected amount)
							 (+ 
							  (chain selected amount)
							  (chain data 0 costs 0 amount)))
							(setf 
							 (chain cost amount)
							 (chain data 0 costs 0 amount))
							(setf
							 (@ cost groupslist)
							 groups)
						(if (getprop 
						     (getprop (chain selected costs) id)
						     "edit")
						    (setf (getprop 
							   (getprop (chain selected costs) id)
							   "edit") NIL)
						    (setf (getprop 
							   (getprop (chain selected costs) id) "edit") t))))))))))
				       ))))

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

      (defun return-lisp-list() arguments))))
