;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun random-circles ()
  (let ((width 640) (height 480))
    (sdl:with-init ()
      (let ((display (sdl:set-window width height)))
	(sdl:set-framerate 0)
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl-gfx:draw-filledCircle display
				      (sdl::point (random width) (random height))
				      (random 100)
				      (sdl::color (random 255) (random 255) (random 255) (random 255)))
	   (sdl:update-surface display)))))))

