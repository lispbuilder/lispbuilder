;; Demonstration/Test of using SDL (Simple Media Layer) library
;; using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones
;; see COPYING for license

;; To run this sample
;; (load "sdl_recursive_rects.lisp")
;; (sdl::recursive-rects) recursively and randomly divides up the screen with rectangles

(in-package :sdl-examples)
  
; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)

; utilities used in this sample
(defun random0-n(n)
  "get a random number from 0 to n"
  (if (= n 0)
      0
    (random n)))

; recursive rectangle demo
(defun draw-recursive-rects (surface_ptr x1 y1 x2 y2 min-size &optional (level 0))
  "Draw rectangles randomly subdividing the given co-ordinates"
  (let
      ((w (abs (- x1 x2)))
       (h (abs (- y1 y2))))
    (let
	((sx (+ x1 (random0-n w)))
	 (sy (+ y1 (random0-n h))))
      (if (or
	   (>= min-size w)
	   (>= min-size h))
	  (let ((color (* (random 256) (expt 256 (mod level 3)))))
	    (sdl::draw-rect-end-points surface_ptr x1 y1 x2 y2 color))
	  (progn
	    (draw-recursive-rects surface_ptr x1 y1 sx sy min-size (1+ level))
	    (draw-recursive-rects surface_ptr sx y1 x2 sy min-size (1+ level))
	    (draw-recursive-rects surface_ptr x1 sy sx y2 min-size (1+ level)) 
	    (draw-recursive-rects surface_ptr sx sy x2 y2 min-size (1+ level)))))))


; main function

(defun recursive-rects()
  (sdl::load-sdl-library)
  "recursively and randomly divides up the screen with rectangles"
  (if (= 0 (sdl::sdl_init (logior sdl::SDL_INIT_AUDIO sdl::SDL_INIT_VIDEO)))
      (let ((surface_ptr (sdl::SDL_SetVideoMode *SCREEN-WIDTH* *SCREEN-HEIGHT* 0 sdl::SDL_ANYFORMAT)))
	(draw-recursive-rects surface_ptr 0 0 *SCREEN-WIDTH* *SCREEN-HEIGHT* 10)
	(format t "video mode set. width ~a height ~a~%" 
		(cffi:foreign-slot-value surface_ptr 'sdl::SDL_Surface 'sdl::w)
		(cffi:foreign-slot-value surface_ptr 'sdl::SDL_Surface 'sdl::h))
	(with-foreign-object (event_ptr 'sdl::SDL_Event)
	  (do
	   ((event-type 0))
	   ((eql event-type sdl::SDL_QUIT))
	    (if (sdl::SDL_PollEvent event_ptr)
		(setf event-type (cffi:foreign-slot-value event_ptr 'sdl::SDL_Event 'sdl::type)))
	    (sdl::SDL_UpdateRect surface_ptr 0 0 0 0)))
	(sdl::SDL_Quit))
      (error "Unable to start SDL~%")))