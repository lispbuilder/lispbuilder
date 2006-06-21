;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun random-circles ()
  (sdl:with-init ()
    (sdl:with-display (640 480 :surface-name display)
      (let ((width (sdl:surf-w display))
	    (height (sdl:surf-h display)))
	(sdl:set-framerate 0)
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl-gfx:draw-filledCircle display
				      (sdl:point (random width) (random height))
				      (random 100)
				      (sdl:color (random 255) (random 255) (random 255) (random 255)))
	   (sdl:update-surface display)))))))
