;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "setup") which just loads the cffi and lispbuilder-sdl packages
;;;; (load "sdl_pixels.lisp")
;;;; (lispbuilder-sdl::pixels) 

(in-package :lispbuilder-sdl) 
  
; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)
(defparameter *display-surface* nil)
(defparameter *loaded-bmps* nil)

; utilities used in this sample

(defun random-color(surface)
  (SDL_MapRGB (foreign-slot-value surface 'SDL_Surface 'format)
	      (random 256)
	      (random 256)
	      (random 256)))


(defun pixel-test(surface)
  (put-pixel surface (random *SCREEN-WIDTH*) (random *SCREEN-HEIGHT*) (random-color surface)))

(defparameter *break-in* nil)

(defparameter *test-color* nil)

(defun pixels()
  "demonstrates put-pixel and get-pixel as well as color and locking functions"
  (load-sdl-library)
  (if (= 0 (sdl_init SDL_INIT_VIDEO))
      (let ((*display-surface* (SDL_SetVideoMode *SCREEN-WIDTH* *SCREEN-HEIGHT* 0 SDL_ANYFORMAT)))
	(unless (null-pointer-p *display-surface*)
	  (format t "SDL_MapRGB test-color in screen format: ~a~%"
		  (SDL_MapRGB (foreign-slot-value *display-surface* 'SDL_Surface 'format) 255 0 0))
	  (with-foreign-object (event_ptr 'SDL_Event)
	    (do
	     ((event-type 0))
	     ((eql event-type SDL_QUIT))
	      (if (SDL_PollEvent event_ptr)
		  (setf event-type (cffi:foreign-slot-value event_ptr 'SDL_Event 'type)))
	      (with-surface-lock *display-surface*
		(put-pixel *display-surface* 10 10 (SDL_MapRGB (foreign-slot-value *display-surface* 'SDL_Surface 'format) 255 0 0))
		(pixel-test *display-surface*))
	      (SDL_UpdateRect *display-surface* 0 0 0 0)
	      (SDL_Flip *display-surface*)))
	  (format t "get-pixel test. pixel at 10 10: ~a~%"
		  (get-pixel *display-surface* 10 10))
	  (SDL_FreeSurface *display-surface*)
	  (setf *display-surface* nil)
	  (SDL_Quit)))
      (error "Unable to start SDL~%")))

