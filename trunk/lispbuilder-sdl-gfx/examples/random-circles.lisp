;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defvar *circles-per-frame* 1)

(defun random-circles ()
  (let ((width 640) (height 480))
    (sdl:with-init ()
      (sdl:window width height)
      (sdl-gfx:initialise-default-font)
      (setf (sdl:frame-rate) 0)
      (fps-init)
      (sdl-gfx:render-string-shaded "Calculating FPS....."
				    sdl:*white*
				    sdl:*black*
				    :free t
				    :cache t)
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
	       (display-fps 10 50 sdl:*default-display*)
	       (sdl:update-display))))))

