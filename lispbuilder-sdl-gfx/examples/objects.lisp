;;;;; Converted from the "Objects" Processing example at:
;;;;; "http://www.processing.org/learning/examples/objects.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defvar *width* 200)
(defvar *height* 200)

(defclass m-rect ()
  ((w :accessor w :initform 0 :initarg :w)
   (xpos :accessor xpos :initform 0 :initarg :xpos)
   (h :accessor h :initform 0 :initarg :h)
   (ypos :accessor ypos :initform 0 :initarg :ypos)
   (d :accessor d :initform 0 :initarg :d)
   (tt :accessor tt :initform 0 :initarg :tt)))

(defvar *r1* (make-instance 'm-rect :w 1 :xpos 134.0 :h 0.532 :ypos (* 0.083 *height*) :d 10.0 :tt 60.0))
(defvar *r2* (make-instance 'm-rect :w 2 :xpos 44.0 :h 0.166 :ypos (* 0.332 *height*) :d 5.0 :tt 50.0))
(defvar *r3* (make-instance 'm-rect :w 2 :xpos 58.0 :h 0.332 :ypos (* 0.4482 *height*) :d 10.0 :tt 35.0))
(defvar *r4* (make-instance 'm-rect :w 1 :xpos 120.0 :h 0.0498 :ypos (* 0.913 *height*) :d 15.0 :tt 60.0))

(defun objects ()
  (let ((mouse-x 0) (mouse-y 0))
    (sdl:with-init ()
      (sdl:with-display (*width* *height* :title-caption "Recursion, from Processing.org")
	(sdl:set-framerate 60)
	(sdl:clear-display :color #(0 0 0))
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display))
	  (:mousemotion (:x x :y y)
			(setf mouse-x x
			      mouse-y y))
	  (:idle ()
		 (sdl:clear-display :color #(0 0 0))
		 (display *r1*)
		 (display *r2*)
		 (display *r3*)
		 (display *r4*)

		 (move-to-x *r1* (- mouse-x (/ *width* 2)) 30)
		 (move-to-x *r2* (mod (+ mouse-x (* *width* 0.05)) *width*) 20)
		 (move-to-x *r3* (/ mouse-x 4) 40)
		 (move-to-x *r4* (- mouse-x (/ *width* 2)) 50)

		 (move-to-y *r1* (+ mouse-y (* *height* 0.1)) 30)
		 (move-to-y *r2* (+ mouse-y (* *height* 0.025)) 20)
		 (move-to-y *r3* (- mouse-y (* *height* 0.025)) 40)
		 (move-to-y *r4* (- *height* mouse-y) 50)
		 
		 (sdl:update-display)))))))

(defmethod move-to-y ((rect m-rect) posy damping)
  (let ((dif (- (ypos rect) posy)))
    (if (> (abs dif) 1)
	(decf (ypos rect) (/ dif damping)))))

(defmethod move-to-x ((rect m-rect) posx damping)
  (let ((dif (- (xpos rect) posx)))
    (if (> (abs dif) 1)
	(decf (xpos rect) (/ dif damping)))))

(defmethod display ((rect m-rect) &key (surface sdl:*default-display*))
  (sdl:with-color (#(255 255 255))
    (dotimes (i (tt rect))
      (sdl-gfx:draw-box (sdl:rectangle (+ (xpos rect)
					  (* i (+ (d rect)
						  (w rect))))
				       (ypos rect)
				       (w rect)
				       (* (h rect)
					  (sdl:surf-h surface)))
			:surface surface))))
