;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (sdl-examples:pixels) 

(in-package #:sdl-examples) 
  
(defun pixels ()
  "demonstrates put-pixel and get-pixel as well as color and locking functions"
  (sdl:with-display (640 480 :flags '(sdl:SDL_ANYFORMAT sdl:SDL_SWSURFACE))
    (let ((width (sdl:surf-w sdl:*default-display*))
	  (height (sdl:surf-h sdl:*default-display*)))
      (sdl:set-framerate 0)		; Unlock the framerate.
      (sdl:with-init ()
	(sdl:with-events ()
	  (:quit () t)
	  (:idle ()
	   (sdl:with-color ((sdl:random-color))
	     (sdl:draw-point :position (sdl:point (random width) (random height))
			     :update-p t :clipping-p t))))))))

