;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defvar *circles-per-frame* 1)

(let* ((frame-values 600)
       (frame-times (make-array frame-values :initial-element 0 :element-type 'fixnum))
       (frame-time-last 0)
       (frame-count 0))
  (declare (type fixnum frame-values frame-time-last frame-count))

  (defun fps-init ()
    (setf frame-count 0
	  frame-time-last (sdl:sdl-get-ticks))
    (dotimes (i frame-values)
      (setf (aref frame-times i) 0)))

  (defun display-fps (surface)
    (declare (optimize (safety 0) (speed 3) (space 1)))
    (let ((get-ticks (sdl:sdl-get-ticks))
          (frames-per-second 0.0))
      (declare (type fixnum get-ticks)
	       (type float frames-per-second))

      (setf (aref frame-times frame-count) (- get-ticks frame-time-last))
      (setf frame-time-last get-ticks)
      (incf frame-count)
      (when (>= frame-count frame-values)
	(setf frame-count 0)
	(dotimes (i frame-values)
	  (incf frames-per-second (aref frame-times i)))
	(setf frames-per-second (the float (/ 1000 (/ frames-per-second frame-values))))
	(sdl:fill-surface (sdl:color) :surface surface :update-p t)
	(sdl-gfx:draw-string-* (format nil "fps : ~d" (coerce frames-per-second 'float))
			     0 0
			     :surface surface
			     :color sdl:*white*))
      surface)))

(defun random-circles ()
  (let ((width 640) (height 480))
    (sdl:with-init ()
      (sdl:window width height)
      (sdl-gfx:gfx-Primitives-Set-Font sdl-gfx:*font-data* 8 8)
      (setf (sdl:frame-rate) 0)
      (fps-init)
      (sdl:with-surface (fps (sdl:create-surface 150 16 :surface sdl:*default-display* :rle-accel t))
	(sdl:with-events ()
			 (:quit-event () t)
			 (:key-down-event (:key key)
					  (if (sdl:key= key :SDL-KEY-ESCAPE)
					      (sdl:push-quit-event)))
			 (:idle ()
				(dotimes (i *circles-per-frame*)
				  (sdl-gfx:draw-filled-Circle-* (random width) (random height) (random 100)
								:color (sdl:color :r (random 255) :g (random 255) :b (random 255) :a (random 255))
								:surface sdl:*default-display*))
				(sdl:blit-surface (display-fps fps) sdl:*default-display*)
				(sdl:update-display)))))))
