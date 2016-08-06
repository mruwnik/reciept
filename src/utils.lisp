(in-package #:reciept)

(defparameter *timezone-offset* (* 1 3600))

(defun get-this-months-costs (user)
  (with-date
      (:month month :year year)
      (postmodern:with-connection (db-params)
        (postmodern:select-dao
            'cost
            (:and
             (:= 'userId user)
             (:< 'amount 0)
             (:> 'timestamp
                 (simple-date:universal-time-to-timestamp
                  (+ (encode-universal-time 0 0 0 1 month year)
                     *timezone-offset*)))
             (:< 'timestamp
                 (simple-date:universal-time-to-timestamp
                  (+ (encode-universal-time 0 0 0 1
                                            (if (< month 12)
                                                (1+ month)
                                                1)
                                            (if (< month 12)
                                                year
                                                (1+ year)))
                     *timezone-offset*))))
            (:desc 'timestamp)))))

(defun get-group-costs(group costs &key (function 'remove-if-not))
  "returns the sum of all costs belonging to the given group"
  (reduce #'(lambda (a b)
              (+ a (amount b)))
          (remove-if-not #'(lambda (cost)
                             (find (make-keyword group)
                                   (get-groups cost)))
                         costs)
          :initial-value 0))

(defun get-by-groups(groups costs &key (exclusive NIL))
  "returns the costs which are or are not in the given groups. whether to return those which are or not is controlled by the exclusive argument"
  (if groups
      (let ((result ()))
        (dolist (group groups (remove-duplicates result))
          (setf result
                (append result
                        (funcall
                         (if exclusive 'remove-if 'remove-if-not)
                         #'(lambda (cost)
                             (find group (get-groups cost)))
                         costs)))))
      costs))

(defun cost-reduce(user &key (groups NIL) (function '+) (field #'amount)
                          (raw-costs (get-this-months-costs user))
                          (initial-value 0)
                          (costs (get-by-groups groups raw-costs)))
  "reduces the given list of costs by calling
function prev (getf current field), where prev is the value calculated for the previous cost"
  (reduce #'(lambda (a b)
              (funcall function a (funcall field b)))
          costs :initial-value initial-value))

(defun get-days-in-month (month)
  (cond
    ((= month 2) 28)
    ((or (= month 4) (= month 6) (= month 9) (= month 11)) 30)
    (T 31)))

(defun get-spending-statistics(user &optional
                                      (currency
                                       (get-user-default-currency user)))
  (with-date
      (:day date :month month)
      (let* ((costs (remove-if-not
                     #'(lambda(cost)(equal (get-currency cost)
                                           currency))
                     (get-this-months-costs user)))
             (total (cost-reduce user :raw-costs costs))
             (rent (cost-reduce user :groups '(:RENT :INVESTMENTS :TAXES :LOANS) :raw-costs costs))
             (get-stat (lambda (groups)
                         (print-money currency
                                      (cost-reduce user :groups groups
                                                        :raw-costs costs))))
             (without-rent (- total rent))
             (avg-cost (/ without-rent date)))
        (append
         `(("total expenditures" . ,(print-money currency total))
           ("non recurring costs" . ,(print-money currency without-rent))
           ("avg. daily expenditure" . ,(print-money currency avg-cost)))
         (loop for comp in (get-compilations user) collecting
                                                   (cons (name comp) (funcall get-stat (read-from-string (expression comp)))))
         `(("expected cost" .
                              ,(format NIL "~a (~a)" (print-money currency (+ rent (* avg-cost (get-days-in-month month))))
                                       (print-money currency (* avg-cost (get-days-in-month month)))))
           )))))

;; ###### helper functions
(defun get-all-groups (user &optional
                              (rows  (postmodern:with-connection (db-params)
                                       (postmodern:select-dao
                                           'cost
                                           (:= 'userId user)))))
  "returns a list of all cost groups for the given user. by default it returns all groups - to only get groups of a subset, pass the subset as the rows argument"
  (remove-duplicates
   (reduce #'(lambda (a b)
               (concatenate 'list a (get-groups b)))
           rows :initial-value ())))

(defun get-user (name password)
  "returns the given user"
  (let ((user (postmodern:with-connection (db-params)
                (first (postmodern:select-dao 'user (:= 'name name))))))
    (when (and user (nth-value 1 (get-password-hash user password)))
      user)))

(defun get-current-user (request)
  (postmodern:with-connection (db-params)
    (postmodern:get-dao 'user (hunchentoot:session-value :userId))))

(defun get-current-user-id (request)
  (let ((user (get-current-user request)))
    (if user (id user) 0)))

(defun check-if-user-exists (username)
  (postmodern:with-connection (db-params)
    (postmodern:select-dao 'user (:= 'name username))))

(defun print-file (file)
  "returns a file dump as a string"
  (concatenate
   'string "<br>"
   (princ-to-string file)
   ":<br/>"
   (when (probe-file file)
     (if (pathname-name (probe-file file))
         (princ-to-string
          (with-open-file (in file)
            (with-standard-io-syntax
              (read in))))
         (princ-to-string
          (loop for f in (directory
                          (concatenate 'string
                                       (princ-to-string file) "/*"))
                collect f))))))

(defun print-money (currency amount)
  "pretty prints mony depending on the given currency"
  (if (eq currency :YEN)
      (format NIL "~d" (round amount))
      (format NIL "~$" amount)))

(defun get-default(str &optional (default NIL))
  (if (and str (not (equal "" str))) str default))
