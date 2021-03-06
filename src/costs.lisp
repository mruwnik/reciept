(in-package #:reciept)

(defun get-url-groups(group-name)
  (mapcar 'make-keyword
	  (mapcar 'cdr (remove-if-not
			#'(lambda (param)
			    (equalp group-name (car param)))
			(hunchentoot:post-parameters
			 hunchentoot:*request*)))))


(hunchentoot:define-easy-handler (costs-handler
				  :uri (get-page-address :show-costs))
    ()
  (redirect :start-page "view" :show-costs))

(hunchentoot:define-easy-handler (reciepts-handler
				  :uri (get-page-address :show-reciepts))
    ()
  (redirect :start-page))

(hunchentoot:define-easy-handler (add-cost-handler
				  :uri (get-page-address :add-cost))
    ((id :request-type :post :parameter-type 'integer)
     (reciept-id :request-type :post :parameter-type 'integer)
     (description :request-type :post :parameter-type 'string)
     (amount :request-type :post :parameter-type #'read-from-string)
     (currency :request-type :post
	       :init-form NIL
	       :parameter-type 'keyword)
     (new-groups :request-type :post :parameter-type
		 #'(lambda (groups)
		     (unless (equal "" groups)
		       (mapcar 'make-keyword (split-str groups ",")))))
     (ajax :request-type :post :parameter-type 'boolean))
  (with-auth (user userid)
    (unless currency (setf currency (get-user-default-currency userid)))
    (let ((groups (append (get-url-groups "groups[]")
			  new-groups)))
      (when (eq :POST (hunchentoot:request-method hunchentoot:*request*))
	(let ((cost (if id
			(edit-cost id description amount
				   :groups (if groups groups '(:NONE)))
			(add-cost description (- amount) userid
				  :currency currency
				  :groups groups
				  :reciept reciept-id))))
	  (if ajax
	      (progn
		(setf (hunchentoot:content-type*) "application/json")
		(edit-operation-result userid NIL (list cost)
				     (get-currency currency)))
	      (redirect :start-page)))))))


(hunchentoot:define-easy-handler (add-reciept-handler
				  :uri (get-page-address :add-reciept))
    ((reciept-desc :request-type :post :parameter-type 'string)
     (currency :request-type :post
	       :init-form (get-currency (get-default-currency))
	       :parameter-type 'keyword)
     (cost-id :request-type :post :parameter-type
	     #'(lambda (list)
		 (let ((result ()))
		   (dolist (val (hunchentoot:post-parameters hunchentoot:*request*))
		     (when (equalp "cost-id" (car val))
			 (setq result (cons (cdr val) result))))
		   result)))
     (reciept-date :request-type :post :parameter-type 'string)
     (reciept-time :request-type :post :parameter-type 'string)
     (reciept-shop :request-type :post :parameter-type 'string)
     (groups :request-type :post :parameter-type #'(lambda(a)
						     (get-url-groups "groups")))
     (new-groups :request-type :post :parameter-type
		 #'(lambda (groups)
		     (unless (equal "" groups)
		       (mapcar 'make-keyword (split-str groups ","))))))
  (with-auth (user userid)
    (when (and (eq :POST (hunchentoot:request-method hunchentoot:*request*))
	       (not (empty reciept-desc cost-id reciept-shop
		      (hunchentoot:post-parameter "description")
		      (parse-integer (hunchentoot:post-parameter "amount")
				     :junk-allowed T))))
      (let* ((timestamp (parse-sql-date reciept-date reciept-time))
	     (receipt (add-reciept reciept-desc userid
			      :printed timestamp :shop reciept-shop)))
	(dolist (id cost-id)
      	  (unless (empty (hunchentoot:post-parameter
				       (format NIL "amount~a" id)))
	    (add-cost (hunchentoot:post-parameter
		       (format NIL "description~a" id))
		      (- (read-from-string
			   (hunchentoot:post-parameter (format NIL "amount~a" id))))
		      userid
		      :currency currency
		      :groups (unless (empty (hunchentoot:post-parameter
					  (format NIL "groups~a" id)))
				(mapcar 'make-keyword
					(split-str
					 (hunchentoot:post-parameter
					  (format NIL "groups~a" id))
					 ",")))
		      :timestamp timestamp
		      :reciept (id receipt))))
	(unless (empty (hunchentoot:post-parameter "amount"))
	  (add-cost (hunchentoot:post-parameter "description")
		    (- (read-from-string
			(hunchentoot:post-parameter "amount")))
		    userid
		    :currency currency
		    :groups (if new-groups
				(append new-groups groups)
				groups)
		    :timestamp timestamp
		    :reciept (id receipt)))))
    (redirect :show-reciepts)))

(hunchentoot:define-easy-handler (get-costs-handler
				  :uri (get-page-address :get-costs))
    ((from :parameter-type 'integer)
     (to :parameter-type 'integer)
     (groups :parameter-type
	     #'(lambda (list)
		 (let ((result ()))
		   (dolist (val (hunchentoot:post-parameters hunchentoot:*request*))
		     (when (equalp "groups" (car val))
		       (setq result (cons (make-keyword (cdr val)) result))))
		   result)))
     (sort-by :parameter-type #'(lambda (str) (if (equal "" str) NIL str)))
     (sort-dir :parameter-type #'(lambda (str) (if (equal "" str) NIL str))))
  (with-auth (user userid)
    (get-costs userid :sort-by sort-by :sort-dir sort-dir)))

(hunchentoot:define-easy-handler (get-reciepts-handler
				  :uri (get-page-address :get-reciepts))
    ((from :parameter-type 'integer)
     (to :parameter-type 'integer)
     (groups :parameter-type
	     #'(lambda (list)
		 (let ((result ()))
		   (dolist (val (hunchentoot:post-parameters hunchentoot:*request*))
		     (when (equalp "groups" (car val))
		       (setq result (cons (make-keyword (cdr val)) result))))
		   result)))
     (sort-by :parameter-type #'(lambda (str) (if (equal "" str) NIL (make-symbol str))))
     (sort-dir :parameter-type #'(lambda (str) (if (equal "" str) NIL str))))
  (with-auth (user userid)
    (get-reciepts userid :sort-by sort-by :sort-dir sort-dir)))

(hunchentoot:define-easy-handler (delete-cost-handler
				  :uri (get-page-address :delete-cost))
    ((id :parameter-type 'integer)
     (ajax :parameter-type 'boolean))
  (with-auth (user userid)
    (deduct-from-funds id)
    (let ((cost (delete-cost userid id)))
      (add-to-undo-list userid cost)
      (if ajax
	  (edit-operation-result userid NIL (list cost)
				 (get-currency cost))
	  (hunchentoot:redirect (get-page-address :show-costs))))))

(hunchentoot:define-easy-handler (delete-reciept-handler
				  :uri (get-page-address :delete-reciept))
    ((id :parameter-type 'integer)
     (ajax :parameter-type 'boolean))
 (with-auth (user userid)
    (let ((reciept (delete-reciept userid id)))
      (if ajax
	  (edit-operation-result userid (list (id reciept)) (costs reciept)
				 (if (costs reciept)
				     (get-currency
				      (first (costs reciept)))
				     (get-user-default-currency userid)))
	  (redirect :show-reciepts)))))

(defun get-add-cost-fields(&optional (model "cost") desc (amount "0"))
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:label "description:")
    (:input :type "text" :name "description" :value desc
	    :ng-model (concatenate 'string model ".description" ))
    (:label "amount:")
    (:input :type "text" :name "amount" :value amount
	    :ng-pattern "/^\\s*-?\\d+(?:\\.\\d{1,2})?\\s*$/"
	    :ng-model (concatenate 'string model ".amount")) :br
    (:label "groups")
    (:div :class "group-checkboxes"
	  (:span :ng-repeat "group in groups"
		 (:input :type "checkbox"
			 :ng-model (concatenate 'string model ".groups[group]")
			 :name "groups" :value "{{group}}")
		 "{{group}}"))
    (:label "new groups:")
    (:input :type "text" :name "new-groups"
	    :ng-model (concatenate 'string model ".newGroups"))))

(defun get-add-cost-form()
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:form :method "post" :action (get-page-address :add-cost)
	   (cl-who:fmt (get-add-cost-fields))
	   :br
	   (:input :type "submit" :value " submit"))))

(defun get-add-reciept-form()
  (with-date (:day date :month month
		   :year year :hour hour :minute minute)
    (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
      (:form :method "post" :action (get-page-address :add-reciept)
	     :id "add-reciept-form" :ng-controller "AddRecieptCtrl"
	     (:div :class "reciept-header"
		   (:label "amount")
		   (:span :id "reciept-amount"
			  "{{ total(newCosts) | number : 2 }}")
		   (:label "description")
		   (:input :type "text" :name "reciept-desc")
		   (:label "shop")
		   (:input :type "text" :name "reciept-shop")
		   (:label "date")
		   (:input :type "text" :id "reciept-date"
			   :name "reciept-date"
			   :placeholder "dd/MM/yyyy"
         :ng-model "newReciept.date"
			   :ng-pattern "/^\\d{2}\\/\\d{2}\\/\\d{4}$/")
		   (:label "time")
		   (:input :type "text" :id "reciept-time"
			   :name "reciept-time"
         :ng-model "newReciept.time"
			   :ng-pattern "/^[012]\\d?:[012345]\\d$/"
         :placeholder "HH:mm")
		   (:label "currency")
		   (cl-who:fmt (get-currencies-selector)))
	     (:div :id "reciept-costs"
	      (:div :ng-repeat "newCost in newCosts"
		    :id "reciept-cost{{newCost.id}}"
		    :class "reciept-cost"
		    (:label "description")
		    (:input :class "cost-desc" :type "text"
			    :value "{{newCost.description}}"
			    :ng-model "newCost.description"
			    :name "description{{newCost.id}}")
		    (:label "amount:")
		    (:input :class "cost-amount" :type "text"
			    :value "{{newCost.amount | number:2}}"
			    :name "amount{{newCost.id}}"
			    :ng-model "newCost.amount"
			    :ng-pattern "/^\\s*-?\\d+(?:\\.\\d{1,2})?\\s*$/")
		    (:span :class "cost-groups"
			   "{{newCost.groups.join(', ')}}")
		    (:input :type "hidden" :name "groups{{newCost.id}}"
			    :value  "{{newCost.groupsstring}}")
		    (:input :type "button" :value "remove cost"
			    :ng-click "removeNewCost($index)")
		    (:input :type "hidden" :value "{{newCost.id}}"
			    :name "cost-id")))
	     (:div :class "cost"
		 (cl-who:fmt (get-add-cost-fields)))
	     (:input :type "button" :id "add-cost" :value "add cost"
		     :ng-click "addCost()" :ng-model "button"
		     :ng-disabled "!(cost.description && cost.amount)")
	     (:input :type "submit" :value " submit" :ng-model "button"
		     :ng-disabled "!((cost.description && cost.amount && newReciept.date && newReciept.time) || (newCosts && newCosts.length > 0))")))))

(defun reciept-headers()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue NIL :indent t)
    (:div :class "header reciept"
	  (:span :class "amount"
;		 :ng-click "reciepts.sortColumn(\"amount\")"
		 "Amount")
	  (:span :class "description"
		 :ng-click "reciepts.sortColumn(\"description\")"
		 "Description")
	  (:span :class "shop"
		 :ng-click "reciepts.sortColumn(\"shop\")"
		 "Shop name")
	  (:span :class "printed"
		 :ng-click "reciepts.sortColumn(\"printed\")"
		 "Date of purchase")
	  (:span :class "action" "Actions"))))

(defun get-angular-reciepts(&key (sort-by NIL) (sort-dir NIL))
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue NIL :indent t)
    (:div :ng-controller "RecieptsListCtrl" :class "reciepts"
	  (cl-who:fmt
	   (reciept-headers))
     (:div
      :infinite-scroll "reciepts.nextPage()"
      :infinite-scroll-disabled "reciepts.busy"
      :infinite-scroll-distance "1"
      (:div :ng-repeat "reciept in reciepts.items | orderBy:recieptsOrder"
       (:div :class "reciept" :id "{{reciept.id}}"
	     :ng-class "{active: reciept.selected}"
	(:div :class "reciept-data"
	      :ng-click "reciepts.toggle($index)"
	      (:span :class "amount"
		     "{{reciept.amount | number:2}} {{reciept.currency}}")
	      (:span :class "description" "{{reciept.description}}")
	      (:span :class "shop" "{{reciept.shop}}")
	      (:span :class "printed"
		     "{{reciept.printed | date:\"dd/MM/yyyy hh:mm\"}}")
	      (cl-who:fmt
	       (get-page-link :delete-reciept `("id" "{{reciept.id}}")
			      "action")))
	(:div
	 :class "costs"
	 (:div :ng-repeat "cost in reciept.costs | orderBy:costsOrder"
	       :id "cost{{cost.id}}" :class "cost"
	       :ng-class "{edit: cost.edit}"
	       (:div :class "cost-data"
		     (:span :class "description" "{{cost.description}}")
		     (:span :class "amount" "{{cost.amount | number:2}} {{cost.currency}}")
		     (:span :class "groups" "{{cost.groupslist.join(', ')}}")
		     (cl-who:fmt
		      (get-page-link :delete-cost `("id" "{{cost.id}}") "delete"))
		     (:button :id "edit-cost"
			      :ng-click "reciepts.toggleEditCost($index)"
			      :class "edit" "edit"))
	       (:div :class "edit-cost"
		     (cl-who:fmt (get-add-cost-fields))
		     (:input :type "hidden" :name "id" :value "{{cost.id}}")
		     (:button :id "cancel"
			      :ng-click "reciepts.toggleEditCost($index)"
			      :class "cancel" "cancel")
		     (:button :id "save-cost"
			      :ng-click "saveCost($index)"
			      :class "save"
			      :ng-model "button"
			      :ng-disabled "!(cost.description && cost.amount)"
			      "save")))
	 (:div :class "cost" :ng-class "{edit: reciept.newCost.edit}"
	       (:div :class "cost-data"
		     (:button :id "edit-cost"
			      :ng-click "reciepts.toggleEditCost()"
			      :class "edit" "add cost"))
	       (:div :class "edit-cost"
		     (cl-who:fmt (get-add-cost-fields "reciept.newCost"))
		     (:input :type "hidden" :name "reciept-id"
			     :value "{{reciept.id}}")
		     (:button :id "cancel"
			      :ng-click "reciepts.toggleEditCost()"
			      :class "cancel" "cancel")
		     (:button :id "save-cost"
			      :ng-click "saveCost()"
			      :class "save"
			      :ng-model "button"
			      :ng-disabled "!(reciept.newCost.description && reciept.newCost.amount)"
			      "save"))))
	(:div :style "clear: both;")))
     (:div :ng-show "reciepts.busy" "Loading data...")))))

(defun get-angular-costs()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue NIL :indent t)
    (:div :ng-controller "CostsListCtrl"
	:class "costs" :id "costs-table"
	(:table
	 :style "width: 100%"
	 (:tr
	  (dolist (item '("id" "description" "amount" "time" "groups" ""))
	    (cl-who:htm
	     (:th :class item
		  (:a :href
		      (format NIL "~a?sort-by=~a&sort-dir=desc"
			      (get-page-address :show-costs)
			      (if (equal item "time")
				  "timestamp" item))
		      (cl-who:fmt item))))))
	(:tr :ng-repeat "cost in costs | orderBy:costsOrder"
	     (dolist (item '("id" "description" "amount" "timestamp" "groups"))
	       (cl-who:htm
		(:td :class item
		     (cl-who:fmt
		   (format NIL (cond
				 ((equal "timestamp" item)
				  "{{cost.~a | date:\"dd/MM/yyyy hh:mm\"}}")
				 ((equal "groups" item)
				  "{{cost.~alist.join(', ')}}")
				 (t "{{cost.~a}}"))
			   item)))))
	     (:td
	      (cl-who:fmt
	       (get-page-link :delete-cost `("id" "{{cost.id}}")))))))))

(defun get-reciepts(&key (userid 0) (sort-by :printed) (sort-dir "desc") (type :json) (limit NIL) (offset 0))
  (cond
    ((equal type :angular)
     (get-angular-reciepts :sort-by sort-by :sort-dir sort-dir))
    ((equal type :json)
     (with-output-to-string (stream)
       (cl-json:encode-json
	(mapcar 'to-json
		(fill-reciepts userid
			       (get-user-reciepts userid
						  :order-by sort-by
						  :order-dir sort-dir
						  :limit limit
						  :offset offset)))
	stream)))))

(defun get-costs (&key (userId 0) (sort-by NIL) (sort-dir NIL) (type :json) (limit NIL) (offset 0))
  (cond
    ((equal type :angular) (get-angular-costs))
    ((equal type :json)
     (with-output-to-string (stream)
       (cl-json:encode-json
	`#( ,@(loop for cost in (if sort-by
	    (if (eq :desc (make-keyword sort-dir))
		(get-user-costs userId
				:order-by (make-keyword sort-by)
				:order-dir :desc
				:limit limit
				:offset offset)	
		(get-user-costs userId
				:order-by (make-keyword sort-by)
				:order-dir :asc
				:limit limit
				:offset offset))
	    (get-user-costs userId))
		 collecting (to-json cost)))
	stream)))))
