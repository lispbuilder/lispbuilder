;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl:bmp_sample) 

(in-package #:sdl-examples) 

(defvar *bmp-path* (or *load-truename* *default-pathname-defaults*))

; utilities used in this sample

(defun bmp-sample ()
  "demonstrates how to manage and display images from .bmp files"
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl::with-surfaces ((sdl-image (sdl:load-image "sdl.bmp" *bmp-path*))
			   (alien-image (sdl:load-image "lisp.bmp" *bmp-path* :key-color #(253 59 251))))

	(sdl:blit-surface sdl-image sdl:*default-display* :dst-rect #(10 10))
	(sdl:blit-surface alien-image sdl:*default-display* :dst-rect #(300 10))

      (sdl:with-events
	(:quit t)
	(:keydown (state scancode key mod unicode)
		  (if (sdl:is-key key :SDLK_ESCAPE)
		      (sdl:push-quitevent)))
	(:idle
	 (sdl:update-screen)))))))

