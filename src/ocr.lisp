;;;; reciept.lisp

(in-package #:reciept)

;;; the data format for a segment is 
;;; (start-cell end-cell avg-ang start-ang switches length)
;;; where length is (/ total-length (dist start-point end-point))
;;; avg-ang is the average angle of each subsegment relative to the previous
;;; start-ang is the starting point of a sin plot which overlayed on a straight line gives an aproximation of this segment (in radians)

(defmacro deg-to-rad (a)
  (* PI (/ a 180.0)))

(defclass segment ()
  ((start-cell
    :initarg :start-cell
    :initform '(1 0)
    :accessor start-cell
    :documentation "the cell from which this segment starts")
   (end-cell
    :initarg :end-cell
    :initform '(1 0)
    :accessor end-cell
    :documentation "the cell where this segment ends")
   (avg-ang
    :initarg :avg-ang
    :initform 0
    :accessor avg-ang
    :documentation "the average angle of each subsequent angle to the previous")
   (start-ang
    :initarg :start-ang
    :initform 0
    :accessor start-ang
    :documentation "the starting point of a sin plot which overlayed on a straight line gives an aproximation of this segment (in radians)")
   (switches
    :initarg :switches
    :initform 0
    :accessor switches
    :documentation "the amount of inflection points on this segment")
   (seg-length
    :initarg :seg-length
    :initform 1.0
    :accessor seg-length
    :documentation "the total length of this segment divided by the distance between the start and end points")))

(defun get-x (coords)
  (first coords))
(defun get-y (coords)
  (second coords))

(defun vector-length(p1 p2)
  (sqrt (+ (expt (- (get-x p2) (get-x p1)) 2)
	   (expt (- (get-y p2) (get-y p1)) 2))))

(defun points-middle(p1 p2)
  (list (/ (+ (get-x p2) (get-x p1)) 2)
	(/ (+ (get-y p2) (get-y p1)) 2)))

(defun get-vector-angle (p1 p2)
  "returns the angle between the vector [p1 p2] and [p1 ((get-x p1) + 1, )get-y p1)] (in rads)"
  (let ((x-len (- (get-x p2)
		  (get-x p1)))
	(y-len (- (get-y p2)
		  (get-y p1))))
    (cond
      ((and (= x-len 0) (= y-len 0))
       0)
      ((= x-len 0)
       (if (> y-len 0)
	   (/ PI 2) (* 3 (/ PI 2))))
      ((= y-len 0)
       (if (> x-len 0)
	   0 PI))
      (t 
       (if (< 0 x-len)
	   (atan (/ y-len x-len))
	   (+ PI (atan (/ y-len x-len))))))))

(defun angled-distance(point angle distance)
  "returns the point at the given distance from the given point at the given angle"
  (list (+ (get-x point) (* distance (cos angle)))
	(+ (get-y point) (* distance (sin angle)))))

(defun in-bounds(canvas point)
  "returns whether the given point is on the given canvas"
  (and (> (get-x point) 0) (> (get-y point) 0)
       (< (get-x point) (array-dimension canvas 0))
       (< (get-y point) (array-dimension canvas 1))))

;; sides-of-the-polygon: (/ 360 (- 180 angle))
;; segments-offset: (/ start-ang (- 90 (/ angle 2)))
;; curve-segments: (- (/ sides-of-the-polygon 2) (* segments-offset 2))
;; total-segments: (* curve-segments switches)
;(defun get-steps (segment)
;  (do* ((steps (* (max (abs (- (car (start-cell segment))
;			       (car (end-cell segment))))
;		       (abs (- (cadr (start-cell segment))
;			       (cadr (end-cell segment)))))
;		  row-size (seg-length segment))))))


(defun new-segment (start end &optional (avg-ang 0) (start-ang 0)
				   (switches 0) (seg-length 1))
  (make-instance 'segment
		  :start-cell start
		  :end-cell end
		  :start-ang start-ang
		  :avg-ang avg-ang
		  :switches switches
		  :seg-length seg-length))

(defparameter *A* `(,(new-segment '(0 1) '(1 3))
		     ,(new-segment '(1 2) '(2 2))
		     ,(new-segment '(1 3) '(3 1))))
(defparameter *S* `(,(new-segment '(0 1) '(3 2) (* 65 (/ PI 180)) 
				 (* -70 (/ PI 180)) 1 1.8)))
(defparameter *C* `(,(new-segment '(3 1) '(3 3) (deg-to-rad 162)
				  (deg-to-rad 70) 0 2.0)))

;(draw-letter *A* (make-array '(31 31) :initial-element " "))
;(draw-letter *C* (make-array '(31 31) :initial-element " "))
;(draw-letter *S* (make-array '(31 31) :initial-element " "))

(defun letter-to-png-array (letter size)
;  (get-png
   (draw-letter 
    letter
    (make-array (list size size)
		:initial-element 0)
    (/ size 4) #'(lambda ()(lambda(a)1))))


(defun find-start-point(canvas &optional (no-pixel 0))
  "finds the first non empty element in the given 2d array. it starts from the top corner (0,0) and then progresses diagonaly, ie.:
(0 1) -> (1 0) -> (0 2) -> (1 1) -> (2 0) -> ...
"
  (let* ((width (array-dimension canvas 0))
	 (height (array-dimension canvas 1)))
    (loop for x from 0 to (+ (1- width) (1- height)) do
	 (loop for y from (min x (1- height)) downto (max (- x width -1) 0) do
	      (when (not (equal (aref canvas (min (- x y) (1- width)) y) 0))
		(return-from find-start-point
		  (list (min (- x y) (1- width)) y)))))))
  

(defun at-segment-end(canvas curr-pos angle distance)
  (= 0 (aref canvas (get-x curr-pos) (get-y curr-pos))))

(defun get-normal-vector(canvas start 
			&optional (pixel-val 1) (no-pixel 0))
  "returns a linear function which overlies the normal vector of the canvas at the given point. the normal is inverted (points inwards)"
  (let* ((pix (apply 'aref canvas (mapcar #'+ start '(0 1))))
	(cells '((0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1) (1 0) (1 1)))
	 (first 
	  (do* ((i 0 (1+ i))
		(curr (nth i cells) (nth i cells)))
	       ((or (>= i (length cells))
		    (not (equal pix 
			       (apply 'aref canvas (mapcar #'+ curr start))))) (1- i))))
	 (last 
	  (do* ((i (1- (length cells)) (1- i))
		(curr (nth i cells) (nth i cells)))
	       ((or (< i 0)
		    (not (equal pix 
				(apply 'aref canvas (mapcar #'+ curr start))))) (1+ i))))
	 (centre-offset (/ (- last first) 2))
	 (direction (if (= 0 (nth-value 1 (truncate centre-offset)))
		 (nth (+ first centre-offset) cells)
		 (mapcar #'(lambda (a b)(/ (+ a b) 2))
			 (nth (- last (floor centre-offset)) cells)
			 (nth (- last (ceiling centre-offset)) cells))))
	 (x2 (mapcar #'+ direction start)))
    (cond
      ((= 0 (get-x direction)) 
       ((lambda(x)(lambda(n)(list x n))) (get-x start)))
      ((= 0 (get-y direction))
       ((lambda(y)(lambda(n)(list n y))) (get-y start)))
      (t
       ((lambda (a point scale neg offset)
	  (let ((b (- (get-y point) (* a (get-x point)))))
	    (lambda (n)
	      (list (* neg (+ (* scale n) offset))
		    (+ (* a neg (+ offset (* n scale))) b)))))
	(/ (- (get-y x2) (get-y start))
	   (- (get-x x2) (get-x start)))
	start
	(if (> (get-y direction) (get-x direction))
	   (abs (/ (get-y direction) (get-x direction)))
	   (abs (/ (get-x direction) (get-y direction))))
	(if (equal pixel-val 
		   (apply 'aref canvas (mapcar #'round x2))) 
	    1
	   (* (get-x direction) (/ -1 (abs (get-x direction)))
	      (get-y direction) (/ -1 (abs (get-y direction)))))
	(if (equal pixel-val 
		   (apply 'aref canvas (mapcar #'round x2)))
	    (get-x start)
	    (* (get-x direction) (/ -1 (abs (get-x direction)))
	       (get-y direction) (/ -1 (abs (get-y direction)))
	       (get-x start))))))))

(defun line-centre(canvas start &optional (no-pixel 0))
  "returns a point lieing in the center of the shape of which 'point' is a border point, plus the function describeing the line it traversed to get to that point. the returned point is the center of a line going from this point to the point which lies on the other side of the figure according to the normal vector in this point"
  (let* ((normal (get-normal-vector canvas start)))
    (do* ((i 1 (1+ i))
	  (point (funcall normal i)
		 (funcall normal i)))
	 ((not (and (in-bounds canvas point)
		    (not (equal no-pixel
				(apply #'aref canvas 
				       (mapcar #'round point))))))
	  (values (mapcar #'(lambda(a b)(/ (+ a b) 2)) point start)
		  normal))
;      (when (in-bounds canvas point)
;	(setf (apply #'aref canvas (mapcar #'round point)) (1+ i))))))
      )))


(defun next-point(canvas curr-pos angle distance &key (empty-marker 0))
  (let ((estimated-point (angled-distance curr-pos angle distance)))
;    (if (= empty-marker 
;	       (aref canvas (round (get-x estimated-point))
;		     (round (get-y estimated-point))))

	(let ((ref-point (mapcar #'round curr-pos)));(angled-distance curr-pos angle distance))))
	  (list (mapcar #'round (angled-distance ref-point (- angle (deg-to-rad 88)) 1))
		(mapcar #'round (angled-distance ref-point (+ angle (deg-to-rad 88)) 1))))
))
;	estimated-point)))

(defun get-perpendicular(p1 p2)
  "returns a function that is perpendicular to a line running through the given 2 points. the returned function also goes through point 2"
  (if (and (= (get-x p1) (get-x p2)) (= (get-y p1) (get-y p2)))
      (lambda (x)(get-y p2))
      (let* ((a (unless (= 0 (- (get-x p1) (get-x p2)))
		  (/ (- (get-y p1) (get-y p2)) (- (get-x p1) (get-x p2)))))
	     (b2 (when a 
		   (+ (get-y p2) (/ (get-x p2) a)))))
	(if a
	    (lambda (n)(+ (- (/ n a)) b2))
	    (lambda(x)(get-x p2))))))

(defun get-linear-fx(a b x)
  (if a (+ (* a x) b) x))

(defun get-linear-a-b(p1 p2)
  (let ((a (unless (= 0 (- (get-x p1) (get-x p2)))
		  (/ (- (get-y p1) (get-y p2)) (- (get-x p1) (get-x p2))))))
	(list a (when a (- (get-y p2) (* (get-x p2) a))))))


(defun next-position(a b point d canvas &optional (dir 1))
"returns (new-a new-b d new-point)
       |      * center
     f |     /  |
       |    /   |ax + bx
       |   *x2  | 
       |  /     |
ax + b | /      | x
-------*/.......*...*p2 
       |point   |

"
  (let* ((x (+ (* dir (get-x point))
	       (/ d 
		  (vector-length 
		   point 
		   (list (* (1+  (get-x point)) dir)
			 (+ (* (1+  (get-x point)) dir a) b))))))
	 (p2 (list x (+ (* a x) b)))
	 (bx (+ (* x (+ a (/ 1 a))) b))
	 (center (get-centre-point canvas (/ -1 a) bx p2))
	 (a2 (/ (- (get-y center) (get-y point))
		(- (get-x center) (get-x point))))
	 (b2 (- (get-y center) (* a2 (get-x center))))
	 (x2 (+ (get-x point) (* (- (get-x center) (get-x point))
				 (/ d (vector-length point center))))))
    (list a2 b2 d (list x2 (+ (* a2 x2) b2)))))

(defun find-edge (canvas f start step &optional (empty-marker 0))
  "goes along the given function till it finds the edge of the segment or end of canvas"
  (do* ((x 1 (+ x step))
	(pos (list (get-x start) (get-y start)) (funcall f x)))
       ((or (not (in-bounds canvas pos))
	    (equal empty-marker (apply 'aref canvas (mapcar 'round pos))))
	pos)))

(defun get-centre-point(canvas a &optional (b 0) point (empty-marker 0)
					   (step (if (and a (> a 1))
						     (/ 1 a) 1)))
  "get the centre of a line f(x) = ax + b cutting through the segment which goes through the given point"
  (let ((f (get-scaled-function 
	    (list (1- (get-x point)) 
		  (get-linear-fx a b (1- (get-x point))))
	    point step)))
    (if (equal (apply 'aref canvas (mapcar 'round point)) empty-marker)
	"handle the casse when we start outside the figure"
	(points-middle (find-edge canvas f point step)
		       (find-edge canvas f point (- step))))))

(defun get-scaled-function(p1 p2 step)
  "returns a linear function going through the 2 given points where x is used to step through values along the vector from f(0) to f(1). each step is scaled to the given scale"
  (if (and (= (get-x p1) (get-x p2)) (= (get-y p1) (get-y p2)))
      (lambda (x)(get-y p2))
      (let* ((a (unless (= 0 (- (get-x p1) (get-x p2)))
		  (/ (- (get-y p1) (get-y p2)) (- (get-x p1) (get-x p2)))))
	     (b (when a 
		   (- (get-y p2) (* (get-x p2) a))))
	     (scale (when a 
		      (/ step (sqrt (+ 1 (* a a))))))
	     (neg (if (< (- (get-x p2) (get-x p1)) 0)
		      -1 1)))
	(cond
	  ((not a) (lambda(x)(list (+ (* neg scale n) (* neg (get-x p1)))
				   (get-x p2))))
	  ((= a 0) (lambda(x)(list (+ (* neg scale n) (* neg (get-x p1)))
				   (get-y p2))))
	  (t (lambda (n)(list (+ (* neg scale n) (* neg (get-x p1)))
			      (+ (* (+ (* neg scale n) (* neg (get-x p1))) a) b))))))))
 
;(defvar *normal*
(let* ((canvas 
;	(make-array (list 10 20)	:initial-element 0))
;	(letter-to-png-array *S* 20))
;       (bla
	#2A((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
	    (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0)
	    (0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0)
	    (0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0)
	    (0 0 0 0 0 1 1 1 1 1 1 0 0 0 1 1 1 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
       (width (array-dimension canvas 0))
       (height (array-dimension canvas 1))
       (start (find-start-point canvas))
       (step 1))
  (multiple-value-bind (point f) (line-centre canvas start)
;    (apply 'get-centre-point canvas 
    (apply 'next-position (append (get-linear-a-b (funcall f -1) start) (list point 2 canvas)))))
;    (setf (apply #'aref canvas (mapcar #'round point)) 8)
;    (funcall (get-scaled-function (list 1 (funcall (get-perpendicular 
;						    (funcall f 0) point) 1))
;		  point 1) 1)))
;   canvas))



;       (line-centre (line-centre canvas start) '(7 3)))
;)

;       (next-point (next-point canvas start (- (/ PI 4)) 1)))
;  (setf (aref canvas (round (get-x start)) (round (get-y start))) 8)
;  next-point)
;(mapcar #'(lambda(point)(setf (aref canvas (get-x point) (get-y point)) 5)) next-point);
;canvas)
;  (setf (aref canvas (round (get-x next-point)) (round (get-y next-point))) 5)
;  canvas)

;  (do* ((angle (- (/ PI 2)) (get-vector-angle curr-pos next-point))
;	(curr-pos (find-start-point canvs) next-point)
;	(next-point (next-point canvas curr-pos angle step)
;		    (next-point canvas curr-pos angle step)))
 ;      ((at-segment-end canvas curr-pos angle step) canvas)))
  


; look n forward
; find middle of line between 2 edges that is at 90ang to current vector
; get angle between middle of that line and current vector
; move n along that vector
; repeat
