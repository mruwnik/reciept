(in-package #:reciept)

;; basic utils
(defun make-keyword (name)
  "returns a keyword made out of the given string"
  (when (> (length name) 0)
    (values (intern (string-upcase (string-trim " " name)) "KEYWORD"))))

(defun random-password (length)
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length collect (aref chars (random (length chars))))
            'string)))

(defun get-password-hash(user password)
  "returns a hash of the given password for the given user. a second value is also returned which specifies whether the resulting hash matches the users password"
  (let ((hash (ironclad:byte-array-to-hex-string
	       (ironclad:digest-sequence
		:sha256 (flexi-streams:string-to-octets
			 (concatenate 'string (salt user) password))))))
    (values hash (equal hash (password user)))))


;;; useful functions
(defun split-str (string &optional (separator " ") (r nil))
  "splits the given string. eg 'a b c  d' becomes ('a' 'b' 'c' '' 'd'). note the extra space between 'c' and 'd'. r is a remainder that gets added to the string"
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
	(cons string r))))

(defun empty(&rest things)
  "checks each of the given things to see if they'ew empty. if all items are empty, then T is returned. empty things are anything that C would think of as false"
  (every 'identity
	  (mapcar #'(lambda(thing)
		     (cond
		       ((not thing) T)
		       ((typep thing 'integer)
			(= 0 thing))
		       ((typep thing 'string)
			(equal 0 (length
				  (string-trim
				   '(#\Space #\Tab #\Newline) thing))))
		       (T NIL))) things)))

;;; time functions
(defmacro with-date ((&key (date (get-universal-time)) (minute (gensym))
			   (second (gensym)) (hour (gensym))
			   (day (gensym)) (month (gensym))
			   (year (gensym)) (day-of-week (gensym))
			   (dst-p (gensym)) (tz (gensym))) &rest body)
  "adds a load of variables representing the various parts of the given timestamp"
  `(multiple-value-bind
	(,second ,minute ,hour ,day ,month ,year ,day-of-week ,dst-p ,tz)
      (decode-universal-time ,date)
     ,@body))

(defun create-date-timestamp (&key (seconds 0) (minutes 0) (hours 0)
		      (days 0) (months 0) (years 0))
  "creates a timestamp that is offset from now by the given values"
  (with-date
      (:second second :minute minute :hour hour
       :day date :month month :year year)
    (encode-universal-time (+ second seconds)
			   (+ minute minutes)
			   (+ hour hours)
			   (+ date days)
			   (+ month months)
			   (+ year years))))

(defun get-days-in-month (&optional month year)
  "returns the amount of days in a month. if the month is not specified, the current month is used"
  (when (or (not month) (not year))
    (with-date
	(:month date-month :year date-year)
      (unless month
	(setf month date-month))
      (unless year
	(setf year date-year))))
  (with-date
      (:date (- (encode-universal-time 0 0 0 1
				(if (< month 12)
				    (1+ month)
				    1)
				(if (< month 12)
				    year
				    (1+ year)))
	 (encode-universal-time 0 0 0 1 month year)
	 (* 60 60 24)) :day day)
    day))

(defun format-time(&optional (timestamp (get-universal-time)))
  "return a string representing the given timestamp"
  (with-date (:date timestamp :day date :month month :year year
			:hour hour :minute minute)
    (format NIL "~2,'0d/~2,'0d/~d ~2,'0d:~2,'0d" date month year hour minute)))

(defun parse-sql-date(date time)
  "parses the given date of the format 'dd/mm/yy' 'HH:MM'. returns a universal-time"
  (let ((date-bits (split-str date "/"))
	(time-bits (split-str time ":")))
    (encode-universal-time 0 (parse-integer (second time-bits))
			   (parse-integer (first time-bits))
			   (parse-integer (first date-bits))
			   (parse-integer (second date-bits))
			   (parse-integer (third date-bits)))))

(defun parse-date(date)
  "retunrs a universal-time from 'dd/mm/yy HH:MM'"
  (apply 'parse-sql-date (split-str date " ")))

