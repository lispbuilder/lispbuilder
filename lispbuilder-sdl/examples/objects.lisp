;;;;; Converted from the "Objects" Processing example at:
;;;;; "http://www.processing.org/learning/examples/objects.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples)

(defvar *objects-width* 200)
(defvar *objects-height* 200)

(defclass m-rect ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (w :accessor w :initarg :w)
   (h :accessor h :initarg :h)
   (d :accessor d :initform 0 :initarg :d)
   (tt :accessor tt :initform 0 :initarg :tt)))

(defvar *r1* (make-instance 'm-rect :w 1 :x 134.0 :h 0.532 :y (* 0.083 *objects-height*) :d 10.0 :tt 60))
(defvar *r2* (make-instance 'm-rect :w 2 :x 44.0 :h 0.166 :y (* 0.332 *objects-height*) :d 5.0 :tt 50))
(defvar *r3* (make-instance 'm-rect :w 2 :x 58.0 :h 0.332 :y (* 0.4482 *objects-height*) :d 10.0 :tt 35))
(defvar *r4* (make-instance 'm-rect :w 1 :x 120.0 :h 0.0498 :y (* 0.913 *objects-height*) :d 15.0 :tt 60))

(defgeneric move-to-y (m-rect posy damping))
(defmethod move-to-y ((m-rect m-rect) posy damping)
  (let ((dif (- (y m-rect) posy)))
    (if (> (abs dif) 1)
	(decf (y m-rect) (/ dif damping)))))

(defgeneric move-to-x (m-rect posx damping))
(defmethod move-to-x ((m-rect m-rect) posx damping)
  (let ((dif (- (x m-rect) posx)))
    (if (> (abs dif) 1)
	(decf (x m-rect) (/ dif damping)))))

(defgeneric display (m-rect &key surface))
(defmethod display ((m-rect m-rect) &key (surface sdl:*default-display*))
  (dotimes (i (tt m-rect))      
    (sdl:draw-box-* (sdl-base:to-int (sdl-base::clamp-to-sshort (+ (x m-rect)
						  (* i (+ (d m-rect)
							  (w m-rect))))))
		    (sdl-base:to-int (sdl-base::clamp-to-sshort (y m-rect)))
		    (sdl-base:to-int (sdl-base::clamp-to-sshort (w m-rect)))
		    (sdl-base:to-int (sdl-base::clamp-to-sshort (* (h m-rect)
						  (sdl:height surface))))
		    :surface surface
		    :color sdl:*white*)))

(defun objects ()
  (let ((mouse-x 0) (mouse-y 0)
	(100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window *objects-width* *objects-height*
                  :title-caption "Objects, from Processing.org")
      (setf (sdl:frame-rate) 60)

      (sdl:clear-display (sdl:color))

      (sdl:initialise-default-font sdl:*font-5x7*)
      (draw-fps "Calculating FPS....."
                10 150 sdl:*default-font* sdl:*default-surface* t)

      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:mouse-motion-event (:x x :y y)
         (setf mouse-x x
               mouse-y y))
	(:idle ()
         (sdl:clear-display sdl:*black*)
         (display *r1*)
         (display *r2*)
         (display *r3*)
         (display *r4*)
	       
         (move-to-x *r1* (- mouse-x (/ *objects-width* 2)) 30)
         (move-to-x *r2* (mod (+ mouse-x (* *objects-width* 0.05)) *objects-width*) 20)
         (move-to-x *r3* (/ mouse-x 4) 40)
         (move-to-x *r4* (- mouse-x (/ *objects-width* 2)) 50)
	       
         (move-to-y *r1* (+ mouse-y (* *objects-height* 0.1)) 30)
         (move-to-y *r2* (+ mouse-y (* *objects-height* 0.025)) 20)
         (move-to-y *r3* (- mouse-y (* *objects-height* 0.025)) 40)
         (move-to-y *r4* (- *objects-height* mouse-y) 50)

         ;; Optimization; Draw the font each frame,
         ;; but only render the font once every 100 frames.
         (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
                   10 150 sdl:*default-font* sdl:*default-surface*
                   (funcall 100-frames-p))

         (sdl:update-display))))))
