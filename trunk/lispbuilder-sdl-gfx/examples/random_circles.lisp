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

  (defun fps-init (number-of-frames)
    (setf frame-values number-of-frames
	  frame-count 0
	  frame-time-last (sdl:SDL_getticks))
    (dotimes (i frame-values)
      (setf (aref frame-times i) 0)))

  (defun display-fps (surface)
    (declare (optimize (safety 0) (speed 3) (space 1)))
    (let ((get-ticks (sdl:SDL_getticks))
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
	(sdl:fill-surface :color #(0 0 0) :surface surface :update-p t)
	(sdl-gfx:draw-string (format nil "fps : ~d" (coerce frames-per-second 'float))
			     :position #(0 0) :surface surface :color #(255 255 255)))
      surface)))

(defun random-circles ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl-gfx:gfxPrimitivesSetFont sdl-gfx:font-data 8 8)
      (let ((width (sdl:surf-w sdl:*default-display*))
	    (height (sdl:surf-h sdl:*default-display*)))
	(sdl:set-framerate 0)
	(fps-init)
	(sdl:with-surfaces-free ((fps (sdl:create-surface 150 8 :surface sdl:*default-display* :accel t)))
	(sdl:with-events ()
	  (:quit () t)
	  (:keydown (:key key)
		    (if (eq key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle ()
	   (dotimes (i *circles-per-frame*)
	     (sdl-gfx:draw-filledCircle (random 100)
					:position (sdl:point (random width) (random height))
					:color (sdl:color (random 255) (random 255) (random 255) (random 255))))
	   (sdl:blit-surface :src (display-fps fps) :dst sdl:*default-display* :dst-rect #(10 260) :free-p nil)
	   (sdl:update-display))))))))
