;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun random-circles ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (let ((width (sdl:surf-w sdl:*default-display*))
	    (height (sdl:surf-h sdl:*default-display*)))
	(sdl:set-framerate 0)
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl:with-default-color ((sdl:color (random 255) (random 255) (random 255) (random 255)))
	     (sdl-gfx:draw-filledCircle (sdl:point (random width) (random height))
					(random 100)))
	   (sdl:update-surface)))))))
