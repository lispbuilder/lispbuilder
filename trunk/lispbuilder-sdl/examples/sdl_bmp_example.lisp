;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl:bmp_sample) 

(in-package #:sdl-examples) 

(defun bmp-sample ()
  (sdl:with-init ()
    (sdl::window 640 480 :title-caption "simple bmp example" :icon-caption "simple bmp example")
    (setf (sdl-base::frame-rate) 10)
    
    (sdl::with-surfaces ((img-1 (sdl::load-image "sdl.bmp" *bmp-path*))
			 (img-2 (sdl::load-image "lisp.bmp" *bmp-path* :key-color (sdl::color :r 253 :g 59 :b 251))))
      (setf img-1.x 10
	    img-1.y 10)
      (sdl::draw-image img-1)

      (setf img-2.x 300
	    img-2.y 10)
      (sdl::draw-image img-2))
    
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl::update-display)))))

