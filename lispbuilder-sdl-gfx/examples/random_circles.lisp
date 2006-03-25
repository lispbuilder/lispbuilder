;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun make-color (surface r g b &optional (a nil))
  (if a
      (sdl::sdl_maprgba (sdl::pixelformat surface) r g b a)
      (sdl::sdl_maprgb (sdl::pixelformat surface) r g b)))

(defun random-circles ()
  (sdl::load-sdl-library)
  (sdl-gfx:load-sdl-gfx-library)
  (format t "Starting.....~%")
  (let ((width 640) (height 480))
    (sdl::with-init ()
      (let ((display (sdl::set-window width height)))
	;; As usual, unlock the frame rate or it will be locked to 30fps by default
	(sdl::set-framerate 0)
	(sdl::with-events
	  (:quit t)
	  (:idle
	   (sdl-gfx:filledCircleColor display
				      (random width)
				      (random height)
				      (random 100)
				      (make-color display (random 255) (random 255) (random 255)))
	   (sdl::update-surface display)))))))

