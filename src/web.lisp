(in-package #:reciept)

(setf *js-string-delimiter* #\")

(defparameter *pages* 
  '(:start-page ("/" "" "home")
    :show-reciepts ("/reciepts" "reciepts" "reciepts")
    :show-costs ("/costs" "costs" "costs")
    :get-reciepts ("/get-reciepts" "reciepts" "reciepts")
    :get-costs ("/get-costs" "costs" "costs")
    :add-cost ("/add-cost" "add a new cost" "add cost")
    :add-reciept ("/add-reciept" "add a new reciept" "add reciept")
    :spending-stats ("/spending-stats" "cosspending statistics" "statistics")
    :stats ("/showStats" "cost statistics" "statistics")
    :graphData ("/get-graph-data" NIL NIL)
    :delete-cost ("/delete-cost" "delete" "delete")
    :delete-reciept ("/delete-reciept" "delete" "delete")
    :undo ("/undo" NIL "undo")
    :save ("/save" NIL NIL)
    :login ("/login" NIL NIL)
    :logout ("/logout" NIL NIL)
    :new-account ("/add-user" "create an account" "sign up")

    :reciepts-controller ("/js/reciepts-controller.js")

    :reciepts-json ("/data/reciepts.json")
    :costs-json ("/data/costs.json")))

(defun get-page-address (page &optional params)
  (format NIL "~a~@[?~{~a=~(~a~)~^&~}~]" 
	  (if (typep page 'symbol)
	      (first (getf *pages* page)) page)
	  params))
(defun get-page-title (page)
  (second (getf *pages* page)))
(defun get-page-link-name (page)
  (third (getf *pages* page)))
(defun get-page-link (page &optional options class id)
  (make-link (get-page-address page options)
	     (get-page-link-name page) class id))

(defun start-server (&key (port 8080) (doc-root (get-document-root)))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port
				    :document-root doc-root)))
(defun add-static-file (uri path)
  (setf hunchentoot:*dispatch-table*
	(append hunchentoot:*dispatch-table* 
		(list (hunchentoot:create-static-file-dispatcher-and-handler uri path)))))

(defun add-static-dir (uri path)
  (setf hunchentoot:*dispatch-table*
	(append hunchentoot:*dispatch-table* 
		(list (hunchentoot:create-folder-dispatcher-and-handler uri path)))))

(defun add-static-uri (uri path)
  (if (pathname (probe-file path))
      (add-static-file uri path)
      (add-static-dir uri path)))

(defun create-regex-dispatcher (regex page-function)
  "Just like tbnl:create-regex-dispatcher except it extracts the matched values
   and passes them onto PAGE-FUNCTION as arguments. You want to be explicit about
   where the slashes go.

   For example, given:
   (defun blog-page (pagenum)
     ... )

   (push (create-regex-dispatcher \"^/blog/page(\\d+)\" #'blog-page)
         tbnl:*dispatch-table*)

   When the url blog/page5 is accessed, blog-page is called with pagenum 
   set to 5."
  (let ((scanner (cl-ppcre:create-scanner regex)))
    (lambda (request)
      (multiple-value-bind (whole-match matched-registers)
          (cl-ppcre:scan-to-strings scanner (tbnl:script-name request))
        (when whole-match
          (lambda ()
            (apply page-function (coerce matched-registers 'list))))))))


;(defparameter *server* (start-server :doc-root (merge-pathnames "public/"		 (asdf:system-source-directory :reciept))))
;(hunchentoot:stop *server*)


(defun redirect(page &rest params)
  "returns a redirect to the given page. params are pairs of name value.
eg. (redirect \"home\" param1 bla param2 ble)"
  (hunchentoot:redirect 
   (format NIL "~a~@[?~{~a=~(~a~)~^&~}~]~@[~:*~:[?~;&~]~a~]"
	   (get-page-address page) 
	   params (hunchentoot:query-string hunchentoot:*request*))))


(defun make-tabs(tabs)
  "makes a html tabs thingy. the tabs should be of the format:
    ((<tab name> is-active <contents>)
     (<tab name> is-active <contents>)
     ...)"
;  (when (and tabs (not (some 'second tabs)))
;    (setf (second (first tabs)) t))
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:div :ng-controller "TabsCtrl"
     :class "tabs-container"
     (:div 
      :class "tabs"
      (do* ((tabs tabs (rest tabs))
	    (tab (first tabs) (first tabs))
	    (id 1 (1+ id)))
	   ((not tabs))
	(cl-who:htm
	 (:div :class "tab"
	       :ng-class (format NIL "{active: tabs[~a].selected}" id)
	       :ng-click (format NIL "showTab(~a)" id)
	       :id (format NIL "tab~a" id) (cl-who:fmt (first tab))))))
     (:div 
      :class "tabs-contents"
      (do* ((tabs tabs (rest tabs))
	    (tab (first tabs) (first tabs))
	    (id 1 (1+ id)))
	   ((not tabs))
	(cl-who:htm
	 (:div :class "tab-contents"
	       :ng-class (format NIL "{active: tabs[~a].selected}" id)
	       :id (format NIL "tab~a-contents" id) (cl-who:fmt (third tab)))))))))

(defun make-link (href text &optional class id)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:a :href (get-page-address href)
	:class class :id id
	(when text (cl-who:fmt text)))))

(defun make-checkbox-list(groups title &optional (name title))
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
      (when groups
	(cl-who:htm (:label (cl-who:fmt title))
		    (:div :class "group-checkboxes"
		    (dolist (group groups)
		      (cl-who:htm (:input :type "checkbox" :name name
					  :value group)
				  (cl-who:fmt (string-downcase group)))))))))

(defun show-login-screen (&optional redirect error new-user)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:form :action (get-page-address (if new-user :new-account :login))
	   :method "POST"
	   (when error 
	     (cl-who:htm (:div :class "error" (cl-who:fmt error))))
	   (:label "username:")
	   (:input :type "text" :name "username") :br
	   (:label "password:") 
	   (:input :type "password" :name "password") :br
	   (when new-user
	     (cl-who:htm 
	      (:label "confirm password:") 
	      (:input :type "password" :name "confirm-password") :br
	      (:label "default currency")
	      (:select :name "currency"
		 (do* ((default-currency (get-default-currency))
		       (currencies (get-all-currencies))
		       (currency (pop currencies) (pop currencies)))
		      ((not currencies))
		   (if (= default-currency (pop currencies))
		       (cl-who:htm
			(:option :value currency :selected "selected"
				 (cl-who:fmt (princ-to-string currency))))
		       (cl-who:htm
			(:option :value currency 
				 (cl-who:fmt (princ-to-string currency)))))))))
	   (:input :type "submit" :value 
		   (if new-user "create account" "log in"))
	   (when redirect
	     (cl-who:htm (:input :type "hidden" :name "redirect"
		   :value redirect))))
    (:div 
     (unless new-user
       (cl-who:fmt (get-page-link :new-account `("redirect" ,redirect)))))))

(defmacro with-auth ((&optional (user (gensym)) (id (gensym))) &rest body)
  `(progn (hunchentoot:start-session)
    (if (hunchentoot:session-value :userId)
	(let ((,user (hunchentoot:session-value :userName))
	      (,id (hunchentoot:session-value :userId)))
	  ,@body)
	(show-login-screen (hunchentoot:request-uri hunchentoot:*request*)))))

(defmacro standard-page (page css scripts &body body)
  (let ((script-name (gensym)))
	 `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
	   (:html :xmlns "http://www.w3.org/1999/xhtml"
		  :xml\:lang "en" 
		  :lang "en"
		  :ng-app "recieptApp"
		  (:head 
		   (:meta :http-equiv "Content-Type" 
			  :content    "text/html;charset=utf-8")
		   (:title (get-page-title ,page))
		   (dolist (,script-name ,css)
		     (cl-who:htm
		      (:link :type "text/css" 
			     :rel "stylesheet"
			     :href ,script-name))))
		  (:body 
		   (cl-who:fmt (make-menu ,page))
		   ,@body
		   (dolist (,script-name ,scripts)
		     (cl-who:htm
		      (:script :src ,script-name
			       :type "text/javascript"))))))))

(defun make-menu (page)
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:div :id "header"
	  (:div :class "right" 
		(hunchentoot:start-session)
		(cl-who:fmt
		 (if (hunchentoot:session-value :userId)
		     (make-link (get-page-address :logout) "logout")
		     (make-link (get-page-address :login) "login"))))
	   (dolist (link '(:stats :show-costs))
	    (cl-who:htm 
	     (:div :class (format NIL "right~:[~; current-page~]" 
				  (eq link page))
		   (cl-who:fmt (get-page-link link))))))))

(hunchentoot:define-easy-handler (undo-handler 
				  :uri (get-page-address :undo))
    ()
  (with-auth (user userid)
     (undo-last-operation userid))
  (redirect  :start-page))

(hunchentoot:define-easy-handler (login-handler 
				  :uri (get-page-address :login))
      ((username :request-type :post :parameter-type 'string)
       (password :request-type :post :parameter-type 'string)
       (redirect :parameter-type 'string))
  (let ((user (when (and username (> (length username) 0))
		    (get-user username password))))
    (if user
	(progn
	  (hunchentoot:start-session)
	  (setf (hunchentoot:session-value :userId) (id user))
	  (setf (hunchentoot:session-value :userName) username)
	  (hunchentoot:redirect (if redirect redirect (get-page-address :start-page))))
	(show-login-screen redirect "wrong user or password"))))

(hunchentoot:define-easy-handler (add-user-handler 
				  :uri (get-page-address :new-account))
    ((username :request-type :post :parameter-type 'string)
     (password :request-type :post :parameter-type 'string)
     (confirm-password :request-type :post :parameter-type 'string)
     (redirect :parameter-type 'string)
     (currency :request-type :post 
	       :init-form (get-currency (get-default-currency))
	       :parameter-type 'keyword))
  (if (eq :POST (hunchentoot:request-method hunchentoot:*request*))
    (cond 
      ((or (not username) (<= (length username) 0) (string= "" username))
       (show-login-screen redirect "the user name is required" T))
      ((not (equal password confirm-password))
       (show-login-screen redirect "passwords don't match" T))
      ((check-if-user-exists username)
       (show-login-screen redirect "the user name is already taken" T))
      (t 
       (let ((user (add-user username password currency)))
	 (hunchentoot:start-session)
	 (setf (hunchentoot:session-value :userId) (id user))
	 (setf (hunchentoot:session-value :userName) username)
	 (hunchentoot:redirect (if redirect redirect (get-page-address :start-page))))))
    (show-login-screen redirect "" T)))

(hunchentoot:define-easy-handler (logout-handler 
				  :uri (get-page-address :logout))
    ()
  (hunchentoot:start-session)
  (hunchentoot:delete-session-value :userId)
  (redirect  :start-page))

(hunchentoot:define-easy-handler (get-spending-stats-handler 
				  :uri (get-page-address :spending-stats))
    ((currency :parameter-type 'make-keyword))
  (with-auth (username userid)
    (with-output-to-string (stream)
      (cl-json:encode-json
       `#( (("currency-id" . ,currency) 
	    ("currency" . ,(get-currency currency))
	    ("values" . ,(mapcar #'(lambda(item)`(("name" . ,(first item))
			 ("data" . ,(rest item))))
		  (get-spending-statistics userid (get-currency currency))))))
       stream))))

(defun get-header (user &optional currency)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:div :class "stats-container"
     (:div :class "total-amount" :style "float: left; margin-right: 50px"
	   (dolist (fund (get-funds user))
	     (cl-who:htm 
	      (:span :class "fund-amount" 
		     :id (format NIL "amount-~(~a~)" (getf fund :currency))
		     (cl-who:fmt (print-money (getf fund :currency)
					      (getf fund :amount))))
	      (:span :class "fund-currency" (cl-who:fmt (symbol-name (getf fund :currency))))
	      (when (getf conversions (getf fund :currency))
		(cl-who:htm
		 (:span :class "converted-value"
			"(" (cl-who:fmt
			 (print-money (getf fund :currency)
				 (* (getf fund :amount)
				    (getf conversions (getf fund :currency))))) ")" ))))))
     (:div :class "spending-info"
	   (dolist (stat (apply 'get-spending-statistics 
				(append (list user) currency)))
	     (cl-who:htm 
	      (:div :class "info" (cl-who:fmt 
				   (format NIL "~a: ~{~a~^, ~}" 
					   (first stat) (list (rest stat))))))))
     (:div :class "change-currency"
	   (cl-who:fmt (apply 'get-currencies-selector
			      (when currency currency)))))))

(defun get-currencies-selector(&optional 
				 (default-currency-id (default-currency 
						       (get-current-user 
							hunchentoot:*request*))))
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:select :name "currency"
	     (do* ((currencies (get-all-currencies))
		   (currency (pop currencies) (pop currencies)))
		  ((not currencies))
	       (if (= default-currency-id (pop currencies))
		   (cl-who:htm
		    (:option :value currency :selected "selected"
			     (cl-who:fmt (princ-to-string currency))))
		   (cl-who:htm
		     (:option :value currency 
			      (cl-who:fmt (princ-to-string currency)))))))))

(defun edit-operation-result(userid reciept-ids costs currency)
  (with-output-to-string (stream)
    (cl-json:encode-json
     `#( (("userid" . ,userid)
	  ("costs" .  ,costs)
	  ("reciepts" . ,reciept-ids)
	  ("amount" . ,(when costs
	     `(("total-amount" . ,(getf
				   (get-funds userid 
					      (get-currency currency))
				   :amount))
	       ("currency" . ,currency))))
	  ))
     stream)))


(hunchentoot:define-easy-handler (start-page-handler 
				  :uri (get-page-address :start-page))
    ((view :init-form :show-reciepts :parameter-type 'keyword))
  (with-auth (username userid)
    (standard-page view 
	'("/css/reciept.css")
	`("https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.1/angular.js"
	  "http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
	  "http://code.jquery.com/ui/1.10.3/jquery-ui.min.js"
	  ,(get-page-address :reciepts-controller))
      (cl-who:fmt (get-header userid))
      (cl-who:fmt (get-add-reciept-form))
      :br :br
      (cl-who:fmt
       (make-link (get-page-address :undo) 
		  (get-page-link-name :undo)
		  (unless (> (avaiable-undos userid) 0) "hidden")
		  "undo-link"))
      (cl-who:fmt
       (make-tabs `(("reciepts" ,(equal :show-reciepts view)
				,(get-reciepts :type :angular))
		    ("costs" ,(equal :show-costs view)
			     ,(get-costs :type :angular))))))))

