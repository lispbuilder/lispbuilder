;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl:bmp_sample) 

(in-package #:sdl-examples) 

;; (defvar *bmp-path* (or *load-truename* *default-pathname-defaults*))

(defvar *bmp-path* (make-pathname :directory (pathname-directory #.(or *compile-file-truename* *load-truename*))))

; utilities used in this sample

(defun bmp-sample ()
  (sdl:with-init ()
    (sdl:with-display (640 480)

      (sdl::with-surface ((sdl:load-image "sdl.bmp" *bmp-path*))
	(sdl:blit-surface :dst-rect #(10 10) :update-p t))
      (sdl:with-surface ((sdl:load-image "lisp.bmp" *bmp-path* :key-color #(253 59 251)))
	(sdl:blit-surface :dst-rect #(300 10) :update-p t))
      
      (sdl:set-framerate 10)
      (sdl:with-events ()
	(:quit () t)
	(:keydown (state scancode key mod unicode)
		  (if (sdl:key= key :SDLK_ESCAPE)
		      (sdl:push-quitevent)))
	(:videoexpose () (sdl:update-display))))))
