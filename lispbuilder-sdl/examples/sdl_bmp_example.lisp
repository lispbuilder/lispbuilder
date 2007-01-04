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
    (sdl::window 320 240 :title-caption "simple bmp example" :icon-caption "simple bmp example")
    (setf (sdl-base::frame-rate) 10)
    
    (sdl::with-surfaces ((img-1 (sdl::convert-surface :surface (sdl::load-image "sdl.bmp" *bmp-path*) :free-p t))
			 (img-2 (sdl::convert-surface :surface (sdl::load-image "lisp.bmp" *bmp-path* :key-color (sdl::color :r 253 :g 59 :b 251)) :free-p t))
			 (img-3 (sdl::convert-surface :surface (sdl::rotate-surface 90 :surface img-2) :key-color (sdl::color :r 253 :g 59 :b 251) :free-p t)))
      (sdl::set-xy img-1 10 10)
      (sdl::draw-image img-1 :surface sdl::*default-display*)

      (sdl::draw-image img-2
		       :surface sdl::*default-display*
		       :position (sdl::point :x 190 :y 10))

      (sdl::draw-image img-3 :surface sdl::*default-display*
		       :position (sdl::point :x 80 :y 100)))

    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl::update-display)))))
