(in-package #:reciept)

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

(defun print-array (array)
  (let ((length (array-dimension array 0))
	(width (array-dimension array 0)))
  (apply 'concatenate 'string 
	 (append (loop for i from 0 to (- (* length width) 1)
	    collecting (concatenate 'string 
				    (when (= 0 (mod i width))
				      (if (= 0 (mod (/ i width) (round (/ width 4))))
					  "<span class=\"axis\">"
					  "<span>"))
				    (when (= 0 (mod i width))
				      (princ-to-string (/ i width)))
				    (row-major-aref array i) 
				    (when (= 0 (mod (1+ i) width))
				      "</span>"))) '("</span>")))))

(defun get-step (start end steps segment)
  (if (< (seg-length segment) 1.01)
      (/ (- end start) steps)
      (/ (- end start) steps)))

(defun get-position (segment step-dist step total-steps coords row-size)
  (with-accessors ((start-cell start-cell)
		   (end-cell end-cell)
		   (avg-ang avg-ang)
		   (start-ang start-ang)
		   (switches switches)
		   (seg-length seg-length))
      segment
    (let* ((current-x (get-x coords))
	   (current-y (get-y coords))
	   (current-angle (third coords))
	   (sina (sin current-angle))
	   (cosa (cos current-angle)))
;      (format t "~$ " (* current-angle (/ 180 PI)))
;     (print (list coords step-dist sina cosa))
      (list (+ current-x (* step-dist cosa))
	    (+ current-y (* step-dist sina))
	    (+ current-angle 
		  ; this is a convulted way to switch angles when it's 
		  ; switch time
	       (/ (if (> switches 0)
		   (* avg-ang 
		      (- 1 (* (mod (floor (/ (* step (1+ switches)) total-steps)) 2) 2)))
		   avg-ang) row-size))))))

(Defun draw-segment (segment canvas &optional (row-size 8) 
				      (symbol-generator #'(lambda()(lambda (progress)'(255 0 0)))))
  "prints the given segment onto the given canvas, using point-symbol to mark points"
  (let* ((points-dist (* (sqrt (+ (expt (- (get-x (start-cell segment))
				 (get-x (end-cell segment))) 2)
			(expt (- (get-y (start-cell segment))
			       (get-y (end-cell segment))) 2)))))
	 (dist (* (if (> points-dist 0) points-dist 1)
		  (seg-length segment)))
	 (steps (round (* dist row-size)))
	 (step-dist (/ (* dist row-size) steps)))
    (print `(,steps ,step-dist))
    (do* ((point-symbol (funcall symbol-generator))
	  (step 0 (+ step 1))
	  (coords (list (* (get-x (start-cell segment)) row-size)
			(* (get-y (start-cell segment)) row-size)
			(+ (get-vector-angle (start-cell segment)
					     (end-cell segment))
			   (start-ang segment)))
		  (get-position segment step-dist step steps coords row-size)))
       ((= step steps) canvas)
      (when (and (> (round (get-x coords)) 0) 
		 (< (round (get-x coords)) (array-dimension canvas 0))
		 (> (round (get-y coords)) 0) 
		 (< (round (get-y coords)) (array-dimension canvas 0)))
	(setf (aref canvas (round (get-x coords))
		    (round (get-y coords)))
; (if (= 0   (mod (floor (/ (* step (1+ (switches segment))) steps)) 2))
;		  point-symbol "*"))))))
	      (funcall point-symbol (/ step steps)))))))

(defun draw-letter (letter canvas &optional (row-size 8) 
				    (point-symbol #'(lambda ()(lambda (a)'(255 0 0)))))
  "prints the given letter onto the given canvas"
  (dolist (segment letter) 
    (draw-segment segment canvas row-size point-symbol))
  canvas)

(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
       :displaced-index-offset (* row (array-dimension arr 1))))

(defun get-png (pic)
  (let* ((width (array-dimension pic 0))
	 (height (array-dimension pic 1))
	 (png (make-instance 'zpng:png 
			    :width width
			    :height height
			    :color-type :grayscale-alpha))
	(image (zpng:data-array png)))
    (loop for y from 1 to (- height 1) do
	 (dotimes (x width)
	   (setf (aref image (- height y) x 1) (aref pic x y))))
    png))

;(defun letter-to-png (letter file size)
;  (zpng:write-png (get-png
;		   (draw-letter 
;		    letter
;		    (make-array (list size size)
;				:initial-element 0) (/ size 4) 255))
;	     file))

(defun png-to-stream (pic stream)
  (let* ((width (array-dimension pic 0))
	 (height (array-dimension pic 1))
	 (png (make-instance 'zpng:pixel-streamed-png
			    :width width
			    :height height
			    :color-type :Truecolor)))
    (zpng:start-png png stream)
    (loop for y from 1 to height do
	 (dotimes (x width)
	   (zpng:write-pixel (aref pic x (- height y)) png)))
    (zpng:finish-png png)))

(defun letter-to-png (letter file size)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :element-type '(unsigned-byte 8))
    (png-to-stream 
     (draw-letter 
      letter
      (make-array (list size size)
		  :initial-element '(0 0 0))
      (/ size 4) '(255 0 0))
     stream)))

(defun get-colour-progresser (start-colour)
  (lambda (progress)
    (let ((colour start-colour)
	  (step (- 255 (apply 'max start-colour))))
      (list (floor (+ (* progress step) (first colour)))
	    (floor (+ (* progress step) (second colour)))
	    (floor (+ (* progress step) (third colour)))))))


(setf *random-state* (make-random-state t))

(hunchentoot:define-easy-handler (get-letter-image-handler 
				  :uri "/get-letter-image")
    ((segment :parameter-type #'read-from-string)
     (size :parameter-type 'integer :init-form 400))
  (setf (hunchentoot:content-type*) "img/png")
  (flexi-streams:with-output-to-sequence (stream :element-type :octet)
    (png-to-stream
     (draw-letter 
      (loop for f in (eval segment)
	 collecting (apply 'new-segment f))
      (make-array (list size size) :initial-element '(220 220 220))
      (/ size 4) #'(lambda ()(get-colour-progresser (list (random 170) (random 170) (random 170)))))
     stream)))

(hunchentoot:define-easy-handler (show-letter-handler 
				  :uri "/show-letter")
    ((segment :parameter-type 'string)
     (size :parameter-type 'string :init-form "400"))
  (standard-page :show-costs
      '("/css/reciept.css") NIL
    (:div "format: "
	  "<br/>-- required:  (start-cell) (end-cell) (avg-ang) <br/>-- optional: (start-ang) (switches) (seg-length)")
    (:form
     (:textarea :name "segment" :rows "10" :cols "80"
		(cl-who:fmt (princ-to-string segment)))
     :br
     (:label "size:") (:input :type "text" :name "size" :value size)
     (:input :type "submit" :value "submit"))
    (:div :class "letter"
	  (:span :class "letter-pic"
		(:img :src 
		      (concatenate 'string "/get-letter-image?segment="
				   segment "&size="size))))))
