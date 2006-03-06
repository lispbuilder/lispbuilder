;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package :sdl-gfx-examples) 

(defun make-color (surface r g b &optional (a nil))
  (if a
      (lispbuilder-sdl::sdl_maprgba (lispbuilder-sdl::pixelformat surface) r g b a)
      (lispbuilder-sdl::sdl_maprgb (lispbuilder-sdl::pixelformat surface) r g b)))

(defun random-circles ()
  (lispbuilder-sdl::load-sdl-library)
  (sdl-gfx:load-sdl-gfx-library)
  (format t "Starting.....~%")
  (let ((width 640) (height 480))
    (lispbuilder-sdl::with-init ()
      (let ((display (lispbuilder-sdl::set-window width height)))
	;; As usual, unlock the frame rate or it will be locked to 30fps by default
	(lispbuilder-sdl::set-framerate 0)
	(lispbuilder-sdl::with-events
	  (:quit t)
	  (:idle
	   (sdl-gfx:filledCircleColor display
				      (random width)
				      (random height)
				      (random 100)
				      (make-color display (random 255) (random 255) (random 255)))
	   (lispbuilder-sdl::update-surface display)))))))

