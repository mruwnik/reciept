(in-package #:reciept)

(hunchentoot:define-easy-handler (show-stats
				  :uri (get-page-address :stats))
    ()
  (with-auth (user id)
      (standard-page :stats
	  '("http://code.jquery.com/ui/1.9.2/themes/smoothness/jquery-ui.css"
	    "/css/reciept.css")
	  '("http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
	    "http://code.jquery.com/ui/1.10.3/jquery-ui.min.js"
	    "http://code.highcharts.com/highcharts.js"
	    "http://code.highcharts.com/modules/exporting.js"
	    "/js/stats.js")
	(:div :class "graphs"
	      (cl-who:fmt (get-graphs id))))))

(hunchentoot:define-easy-handler (get-graphs-handler
				  :uri (get-page-address :graphData))
    ((timestep :parameter-type 'integer :init-form 1)
     (timeunit :parameter-type 'string :init-form "day")
     (from :parameter-type 'parse-date
	   :init-form (create-date-timestamp :months -1))
     (to :parameter-type 'parse-date
	 :init-form (create-date-timestamp :months 1)))
  (with-auth (user userid)
    (let* ((timestep (* timestep
			(cond
			  ((equalp "hour" timeunit) (* 60 60))
			  ((equalp "day" timeunit) (* 60 60 24))
			  ((equalp "week" timeunit) (* 60 60 24 7))
			  ((equalp "month" timeunit) (* 60 60 24 31))
			  ((equalp "year" timeunit) (* 60 60 24 365 ))
			  (t (* 60 60 24)))))
	   (rows
	    (get-user-costs
	     userid :where (:and (:> 'timestamp
				   (simple-date:universal-time-to-timestamp from))
			       (:< 'timestamp
				   (simple-date:universal-time-to-timestamp to)))
	     :order-by 'timestamp :order-dir :asc))
	   (groups (get-all-groups userid rows)))
      (when rows
	(get-graph-settings
	 (do* ((current (+ (get-uni-timestamp (first rows)) timestep)
			(incf current timestep))
	       (last (+ (get-uni-timestamp (first (last rows)))
			(* 2 timestep)))
	       (result NIL))
	      ((> current last) result)
	   (setf result (append result
				`(,(with-date (:date current :day date :month month)
					      (format NIL "~2,'0d.~2,'0d" date month))))))

	 (let* ((json-data NIL)
		(data (get-costs-over-time-by-group rows timestep groups)))
	   (dolist (group groups json-data)
	     (setf json-data (append json-data
				     `(((:name . ,group)
					(:data . ,(getf data group))))))))
	 "costs over time" "column" "cost")))))

(defun get-graph-settings (categories data
			   graph-title graph-type yaxis-text)
  (with-output-to-string (stream)
    (cl-json:encode-json
     `#( (("title" . (("text" . ,graph-title)))
	("chart" . (("type" . ,graph-type)))
	("xAxis" . (("id" . "1") (:categories . ,categories)))
	("yAxis" . (("reversed" . t) (:title ("text" . ,yaxis-text))))
	("legend" . (("backgroundColor" . "#FFFFFF")
		    (reversed . T)))
	("plotOptions" . ((:series ("stacking" . "normal"))))
	("series" . ,data)
	))
     stream)))

(defun get-graphs (userid &key (timestep 1) (timeunit "day")
		     (start (create-date-timestamp :months -1))
		     (end (create-date-timestamp :months 1)))
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:div :id "container" :style "min-width: 310px; height: 400px; margin: 0 auto")
    (:div :class "graph-scale"
	  (:label "timestep")
	  (:input :type "text" :id "timestep" :value userid)
	  (:select :name "timestep-unit"
		   (dolist (unit '("hour" "day" "week" "month" "year"))
		      (if (equalp unit timeunit)
			  (cl-who:htm
			   (:option :value unit
				    :selected "selected" (cl-who:fmt unit)))
			  (cl-who:htm
			   (:option :value unit (cl-who:fmt unit)))))))
    (:div :class "graph-limits"
	  (:div :id "from"
		(:label "from")
		(with-date (:date start :day date :month month
				  :year year :hour hour :minute minute)
		  (cl-who:htm
		  (:input :type "text" :id "graph-start-date"
			  :value (format NIL "~2,'0d/~2,'0d/~d"
					 date month year))
		  (:input :type "text" :id "graph-start-time"
			  :value (format NIL "~2,'0d:~2,'0d"
					 hour minute)))))
	  (:div :id "to"
		(:label "to")
		(with-date (:date end :day date :month month
				  :year year :hour hour :minute minute)
		  (cl-who:htm
		   (:input :type "text" :id "graph-end-date"
			   :value (format NIL "~2,'0d/~2,'0d/~d"
					  date month year))
		   (:input :type "text" :id "graph-end-time"
			   :value (format NIL "~2,'0d:~2,'0d"
					  hour minute))))))))

(defun get-costs-over-time-by-group (rows timestep groups)
  "gets a plist of the format <group>: <list of costs>, where <list of costs> is a list containing the sum of all costs during a given period - the period is defined by timestep"
  (do* ((current (get-uni-timestamp (first rows))
		 (incf current timestep))
	(last (+ (get-uni-timestamp (first (last rows))) timestep))
	(result (let ((data NIL))
		  (dolist (group groups)
		    (setf (getf data group) NIL))
		  data)))
       ((>= current last) result)
    (dolist (group groups)
      (setf (getf result group) (append (getf result group) `(,(+ 0)))))
      (do* ((row (pop rows) (pop rows))
	    (groups-len (when row (length (get-groups row)))))
	   ((or (not row)
		(when rows (> (get-uni-timestamp (first rows)) current))))
	(dolist (group (get-groups row))
	  (incf (first (last (getf result group)))
		(/ (amount row) groups-len))))))

(defun get-group-costs-over-time (&key (timestep (* 60 60 24))
				    (rows (select :costs :order-by :timestamp :order-dir "asc")))
  "returns a hash map of <timestamp> : <costs for that period by groups>, where the period is defined by timestep and rows define the data to be used. the data must be sorted chronologicaly"
  (do* ((current (+ (getf (first rows) :timestamp) timestep)
		 (incf current timestep))
	(last (+ (getf (first (last rows)) :timestamp) timestep))
	(base-list (let ((base NIL))
		     (dolist (group (get-groups rows))
		       (setf base (append base `(,group 0))))
		     base))
	(current-list (copy-seq base-list) (copy-seq base-list))
	(result (make-hash-table)))
       ((> current last) result)
    (when (< (getf (first rows) :timestamp) current)
      (do* ((row (pop rows) (pop rows))
	    (groups-len (length (getf row :groups))))
	   ((or (not (first rows))
		(> (getf (first rows) :timestamp) current)))
	(dolist (group (getf row :groups))
	  (incf (getf current-list group) (/ (getf row :amount) groups-len)))))
    (setf (gethash current result) `(,current-list))))
