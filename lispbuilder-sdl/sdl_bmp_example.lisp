;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "setup") which just loads the cffi and lispbuilder-sdl packages
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl::bmp_sample) 

(in-package :lispbuilder-sdl) 
  
; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)
(defparameter *display-surface* nil)
(defparameter *loaded-bmps* nil)

; utilities used in this sample

; TODO If you want to take advantage of hardware colorkey or alpha blit acceleration, you should set the colorkey and alpha value before calling this.
(defun load-bmps(lst)
  "loads in the list of filenames (must be bmp files), and creates a display surface friendly surface for each of them"
  (loop for filename in lst do
	(let ((bmp-surface (load-bmp filename)))
	  (let ((display-surface (convert-surface-to-display-format bmp-surface)))
	    (setf *loaded-bmps* (cons display-surface *loaded-bmps*))
	    (SDL_FreeSurface bmp-surface)))))

(defun close-bmps()
  "free up the surfaces we loaded the bmps into"
  (loop for surface in *loaded-bmps* do
	(SDL_FreeSurface surface))
  (setf *loaded-bmps* nil))

(defun bmp_sample()
  "demonstrates how to manage and display images from .bmp files"
  (load-sdl-library)
  (if (= 0 (sdl_init (logior SDL_INIT_AUDIO SDL_INIT_VIDEO)))
      (let ((*display-surface* (SDL_SetVideoMode *SCREEN-WIDTH* *SCREEN-HEIGHT* 0 SDL_ANYFORMAT)))
	(load-bmps '("sdl.bmp" "lisp.bmp"))
	(let ((dest-rect-1 (make-sdl-rect 0 50 100 100))
	      (dest-rect-2 (make-sdl-rect 200 50 100 100)))
	  (with-foreign-object (event_ptr 'SDL_Event)
	    (do
	     ((event-type 0))
	     ((eql event-type SDL_QUIT))
	      (if (SDL_PollEvent event_ptr)
		  (setf event-type (cffi:foreign-slot-value event_ptr 'SDL_Event 'type)))
	      (blit-surface (first *loaded-bmps*) *display-surface* 10 10)
	      (blit-surface (second *loaded-bmps*) *display-surface* 300 10)
	      (SDL_UpdateRect *display-surface* 0 0 0 0)
	      (SDL_Flip *display-surface*))))
	(close-bmps)
	(SDL_FreeSurface *display-surface*)
	(setf *display-surface* nil)
	(SDL_Quit))
      (error "Unable to start SDL~%")))


