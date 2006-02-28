;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

;;;; To run these samples
;;;; (load "setup") which just loads the cffi and lispbuilder-sdl packages
;;;; (load "random-rects.lisp")
;;;; (sdl::random-rects1)
;;;; (sdl::random-rects2)
;;;; (sdl::random-rects3)

(in-package :lispbuilder-sdl) 
  
(defun random-rects1 ()
  (load-sdl-library)
  (format t "Starting.....~%")
  (let ((width 640) (height 480)
	(rectangle (new-rect)))
    (with-init ();Initialize Systems
      (set-framerate 0) ; Unlock the framerate.
      (let ((display (set-window width height :flags SDL_SWSURFACE)))
	(with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (is-key key :SDLK_ESCAPE)
			(push-quitevent)))
	  (:idle
	   ;Update only the portion of the display that has been written to.
	   (update-surface display :template (draw-random-rect display
							       (+ 1 (random width))
							       (+ 1 (random height))
							       rectangle))))))
    (cffi:foreign-free rectangle)))

(defun random-rects2 ()
  (let ((width 640) (height 480)
	(rectangle (new-rect)))
    (load-sdl-library)
    (format t "Starting.....~%")
    (with-init ()			;Initialize Systems
      (let ((display-surface (set-window width height)))
	(set-framerate 0)
	(with-events	;Main SDL loop / Message Pump
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (is-key key :SDLK_ESCAPE)
			(push-quitevent)))
	  (:idle
	   ;; Set up the random rectangle
	   (setf (rect-x rectangle) (random width)
		 (rect-y rectangle) (random height)
		 (rect-w rectangle) (random width)
		 (rect-h rectangle) (random height))
	   
	   ;; 'Render' the rectangle to the display by
	   ;; filling the display with a random color
	   ;; using the rectangle as a template. This is faster than
	   ;; redrawing the entire window/screen.
	   (fill-surface display-surface (random 256) (random 256) (random 256) 
			 :template rectangle :update-surface t)))))
    (cffi:foreign-free rectangle)))

(defun random-rects3 ()
  (load-sdl-library)
  (format t "Starting.....~%")
  (let ((width 640) (height 480))
    (with-init ();Initialize Systems
      (set-framerate 0) ; Unlock the framerate.
      (let ((display (set-window width height :flags '(SDL_ANYFORMAT SDL_SWSURFACE))))
	(with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (is-key key :SDLK_ESCAPE)
			(push-quitevent)))
	  (:idle
	   (draw-random-rect display (+ 1 (random width)) (+ 1 (random height)))
	   ;Update the entire display
	   (update-surface display)))))))

