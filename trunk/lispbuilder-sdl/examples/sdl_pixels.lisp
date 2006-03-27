;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "setup") which just loads the cffi and lispbuilder-sdl packages
;;;; (load "sdl_pixels.lisp")
;;;; (lispbuilder-sdl::pixels) 

(in-package #:sdl-examples) 
  
; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)
(defparameter *display-surface* nil)
(defparameter *loaded-bmps* nil)

; utilities used in this sample

(defun random-color(surface)
  (sdl::SDL_MapRGB (cffi:foreign-slot-value surface 'sdl::SDL_Surface 'sdl::format)
		   (random 256)
		   (random 256)
		   (random 256)))


(defun pixel-test(surface)
  (sdl::put-pixel surface (random *SCREEN-WIDTH*) (random *SCREEN-HEIGHT*) (random-color surface)))

(defparameter *break-in* nil)

(defparameter *test-color* nil)

(defun pixels()
  "demonstrates put-pixel and get-pixel as well as color and locking functions"
  (if (= 0 (sdl::sdl_init sdl::SDL_INIT_VIDEO))
      (let ((*display-surface* (sdl::SDL_SetVideoMode *SCREEN-WIDTH* *SCREEN-HEIGHT* 0 sdl::SDL_ANYFORMAT)))
	(unless (cffi:null-pointer-p *display-surface*)
	  (format t "SDL_MapRGB test-color in screen format: ~a~%"
		  (sdl::SDL_MapRGB (cffi:foreign-slot-value *display-surface* 'sdl::SDL_Surface 'sdl::format) 255 0 0))
	  (with-foreign-object (event_ptr 'sdl::SDL_Event)
	    (do
	     ((event-type 0))
	     ((eql event-type sdl::SDL_QUIT))
	      (if (sdl::SDL_PollEvent event_ptr)
		  (setf event-type (cffi:foreign-slot-value event_ptr 'sdl::SDL_Event 'sdl::type)))
	      (sdl::with-surface-lock *display-surface*
		(sdl::put-pixel *display-surface* 10 10 (sdl::SDL_MapRGB (cffi:foreign-slot-value *display-surface* 'sdl::SDL_Surface 'sdl::format) 255 0 0))
		(pixel-test *display-surface*))
	      (sdl::SDL_UpdateRect *display-surface* 0 0 0 0)
	      (sdl::SDL_Flip *display-surface*)))
	  (format t "get-pixel test. pixel at 10 10: ~a~%"
		  (sdl::get-pixel *display-surface* 10 10))
	  (sdl::SDL_FreeSurface *display-surface*)
	  (setf *display-surface* nil)
	  (sdl::SDL_Quit)))
      (error "Unable to start SDL~%")))

