(in-package #:reciept)

(defclass user ()
  ((id
    :initarg :id
    :reader id
    :col-type serial
    :documentation "the id of this user")
   (name
    :initarg :name
    :initForm (error "must supply a userName" )
    :accessor name
    :col-type varchar
    :documentation "the name of this user")
   (password
    :initarg :password
    :initForm ""
    :accessor password
    :col-type varchar
    :documentation "this users password")
   (salt
    :initarg :salt
    :initForm (random-password 15)
    :accessor salt
    :col-type varchar
    :documentation "this users password salt")
   (defaultCurrency
    :initarg :default-currency
    :initForm (get-default-currency)
    :accessor default-currency
    :col-type integer
    :documentation "the default currency of this cost")
   (created
    :initarg :created
    :initForm (simple-date:universal-time-to-timestamp (get-universal-time))
    :accessor created
    :col-type timestamp
    :documentation "when this user was created"))
  (:documentation "Dao class for a users record.")
  (:metaclass postmodern:dao-class)
  (:table-name users)(:keys id))

(defclass reciept ()
  ((id
    :initarg :id
    :reader id
    :col-type serial
    :documentation "the id of this reciept")
   (userId
    :initarg :userId
    :initForm (error "must supply a userId" )
    :accessor userId
    :col-type integer
    :documentation "the id of this costs owner")
   (description
    :initarg :description
    :initForm ""
    :accessor description
    :col-type varchar
    :documentation "a short description of this reciept")
   (shop
    :initarg :shop
    :initForm ""
    :accessor shop
    :col-type varchar
    :documentation "where this reciept is from")
   (costs
    :initarg :costs
    :initForm '()
    :accessor costs
    :documentation "this reciepts costs")
   (printed
    :initarg :printed
    :initForm (simple-date:universal-time-to-timestamp (get-universal-time))
    :accessor printed
    :col-type timestamp
    :documentation "the date when this reciept was printed")
   (added
    :initarg :added
    :initForm (simple-date:universal-time-to-timestamp (get-universal-time))
    :accessor added
    :col-type timestamp
    :documentation "when this reciept was added to the database"))
  (:documentation "Dao class for a reciept.")
  (:metaclass postmodern:dao-class)
  (:table-name reciepts)(:keys id))

(defclass cost ()
  ((id
    :initarg :id
    :reader id
    :col-type serial
    :documentation "the id of this cost")
   (userId
    :initarg :userId
    :initForm (error "must supply a userId" )
    :accessor userId
    :col-type integer
    :documentation "the id of this costs owner")
   (description
    :initarg :description
    :initForm ""
    :accessor description
    :col-type (or postmodern:db-null string)
    :documentation "describes this cost")
   (amount
    :initarg :amount
    :initForm 0
    :accessor amount
    :col-type numeric
    :documentation "the actual cost")
   (currency
    :initarg :currency
    :initForm (get-default-currency)
    :accessor currency
    :col-type integer
    :documentation "the currency of this cost")
   (timestamp
    :initarg :timestamp
    :initForm (simple-date:universal-time-to-timestamp (get-universal-time))
    :accessor timestamp
    :col-type timestamp
    :documentation "when this cost was added")
   (groups
    :initarg :groups
    :initForm '()
    :accessor groups
    :col-type text
    :documentation "which group this cost belongs to")
   (reciept
    :initarg :reciept
    :initForm 0
    :accessor reciept
    :col-type integer
    :documentation "the reciept to which this cost is attached"))
  (:documentation "Dao class for a costs record.")
  (:metaclass postmodern:dao-class)
  (:table-name costs)(:keys id))

(defun get-groups (cost)
  (when cost
    (if (stringp (groups cost))
	(setf (groups cost) (read-from-string (groups cost)))
	(groups cost))))

(defun get-uni-timestamp (cost)
  "returns the given costs timestamp as a simple-date:timestamp"
  (simple-date:timestamp-to-universal-time (timestamp cost)))

(defclass compilation()
  ((id
    :initarg :id
    :reader id
    :col-type serial
    :documentation "the id of this compilation")
   (userId
    :initarg :userId
    :accessor userId
    :col-type integer
    :documentation "the id of the user to whom this belongs")
   (name
    :initarg :name
    :accessor name
    :col-type varchar
    :documentation "the name of this compilation")
   (expression
    :initarg :expression
    :accessor expression
    :col-type varchar
    :documentation "the expression used to generate this compilation")
   (orderNum
    :initarg :orderNum
    :initform 0
    :accessor orderNum
    :col-type integer
    :documentation "defines where to display this compilation - the lower the number, the sooner it will appear in a list of compilations"))
  (:documentation "Dao class for a compliation record.")
  (:metaclass postmodern:dao-class)
  (:table-name compilations)(:keys id))

