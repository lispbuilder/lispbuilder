;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defvar *circles-per-frame* 1)

(defun random-circles ()
  (let ((width 640) (height 480)
	(100-frames-p (every-n-frames 100)))
    (sdl:with-init ()
      (sdl:window width height)
      (setf (sdl:frame-rate) 0)
      
      (sdl-gfx:initialise-default-font)
      (draw-fps "Calculating FPS....." 10 50 sdl-gfx:*default-font* sdl:*default-surface* t)

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
	       (draw-fps (format nil "FPS : ~2$" (sdl:average-fps))
			 10 50 sdl-gfx:*default-font* sdl:*default-surface*
			 (funcall 100-frames-p))
	       (sdl:update-display))))))

