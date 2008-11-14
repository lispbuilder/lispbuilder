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

(defvar *r1* (make-instance 'm-rect :w 1 :xpos 134.0 :h 0.532 :ypos (* 0.083 *height*) :d 10.0 :tt 60))
(defvar *r2* (make-instance 'm-rect :w 2 :xpos 44.0 :h 0.166 :ypos (* 0.332 *height*) :d 5.0 :tt 50))
(defvar *r3* (make-instance 'm-rect :w 2 :xpos 58.0 :h 0.332 :ypos (* 0.4482 *height*) :d 10.0 :tt 35))
(defvar *r4* (make-instance 'm-rect :w 1 :xpos 120.0 :h 0.0498 :ypos (* 0.913 *height*) :d 15.0 :tt 60))

(defmethod move-to-y ((rect m-rect) posy damping)
  (let ((dif (- (ypos rect) posy)))
    (if (> (abs dif) 1)
	(decf (ypos rect) (/ dif damping)))))

(defmethod move-to-x ((rect m-rect) posx damping)
  (let ((dif (- (xpos rect) posx)))
    (if (> (abs dif) 1)
	(decf (xpos rect) (/ dif damping)))))

(defmethod display ((rect m-rect) &key (surface sdl:*default-display*))
  (dotimes (i (tt rect))
    (sdl-gfx:draw-box-* (sdl-base:to-int (sdl-base::clamp-to-sshort (+ (xpos rect)
								       (* i (+ (d rect)
									       (w rect))))))
			(sdl-base:to-int (sdl-base::clamp-to-sshort (ypos rect)))
			(sdl-base:to-int (sdl-base::clamp-to-sshort (w rect)))
			(sdl-base:to-int (sdl-base::clamp-to-sshort (* (h rect)
								       (sdl:height surface))))
			:surface surface
			:color sdl:*white*)))

(defun objects ()
  (let ((mouse-x 0) (mouse-y 0)
	(100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window *width* *height* :title-caption "Objects, from Processing.org")
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (sdl:color))

      (sdl-gfx:initialise-default-font)
      (draw-fps "Calculating FPS....." 10 150 sdl:*default-font* sdl:*default-display* t)
      
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:mouse-motion-event (:x x :y y)
			     (setf mouse-x x
				   mouse-y y))
	(:idle ()
	       (sdl:clear-display (sdl:color))
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

	       (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
			 10 150 sdl:*default-font* sdl:*default-display*
			 (funcall 100-frames-p))
		 
	       (sdl:update-display))))))
