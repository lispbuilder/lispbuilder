;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

;;;; To run these samples
;;;; (load "setup") which just loads the cffi and lispbuilder-sdl packages
;;;; (load "random-rects.lisp")
;;;; (sdl:random-rects1)
;;;; (sdl:random-rects2)
;;;; (sdl:random-rects3)

(in-package #:sdl-examples) 
  
(defun random-rects1 ()
  (let ((width 640) (height 480)
	(rectangle (sdl:new-rect)))
    (sdl:with-init ();Initialize Systems
      (sdl:set-framerate 0) ; Unlock the framerate.
      (let ((display (sdl:set-window width height :flags sdl:SDL_SWSURFACE)))
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   ;Update only the portion of the display that has been written to.
	   (sdl:update-surface display :template (sdl:draw-random-rect display
									 (+ 1 (random width))
									 (+ 1 (random height))
									 rectangle))))))
    (cffi:foreign-free rectangle)))

(defun random-rects2 ()
  (let ((width 640) (height 480)
	(rectangle (sdl:new-rect)))
    (sdl:with-init ()			;Initialize Systems
      (let ((display-surface (sdl:set-window width height)))
	(sdl:set-framerate 0)
	(sdl:with-events	;Main SDL loop / Message Pump
	    (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   ;; Set up the random rectangle
	   (setf (sdl:rect-x rectangle) (random width)
		 (sdl:rect-y rectangle) (random height)
		 (sdl:rect-w rectangle) (random width)
		 (sdl:rect-h rectangle) (random height))
	   
	   ;; 'Render' the rectangle to the display by
	   ;; filling the display with a random color
	   ;; using the rectangle as a template. This is faster than
	   ;; redrawing the entire window/screen.
	   (sdl:fill-surface display-surface (random 256) (random 256) (random 256) 
			      :template rectangle :update-surface t)))))
    (cffi:foreign-free rectangle)))

(defun random-rects3 ()
  (let ((width 640) (height 480))
    (sdl:with-init ();Initialize Systems
      (sdl:set-framerate 0) ; Unlock the framerate.
      (let ((display (sdl:set-window width height :flags '(sdl:SDL_ANYFORMAT sdl:SDL_SWSURFACE))))
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:draw-random-rect display (+ 1 (random width)) (+ 1 (random height)))
	   ;Update the entire display
	   (sdl:update-surface display)))))))

