;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (sdl-examples:pixels) 

(in-package #:sdl-examples) 
  
(defun pixels ()
  "demonstrates put-pixel and get-pixel as well as color and locking functions"
  (let ((width 640) (height 480))
    (sdl:set-framerate 0) ; Unlock the framerate.
    (sdl:with-init ()
      (let ((display-surface (sdl:set-window width height :flags '(sdl:SDL_ANYFORMAT sdl:SDL_SWSURFACE))))
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl:draw-pixel display-surface (sdl::point (random width) (random height))
			   (sdl::random-color display-surface)
			   :check-lock-p t :update-p t :clipping-p t)))
	(format t "Pixel at (10, 10): ~a~%" (sdl:get-pixel display-surface #(10 10)))
	(sdl::FreeSurface display-surface)))))

(sdl:set-framerate 0)