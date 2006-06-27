;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun random-circles ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl-gfx:gfxPrimitivesSetFont sdl-gfx:font-data 8 8)
      (let ((width (sdl:surf-w sdl:*default-display*))
	    (height (sdl:surf-h sdl:*default-display*))
	    (fps-ticks 0) (fps-frame-count 0) (fps-text "") (fps-average-window 500))
	(sdl:set-framerate 0)
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:with-default-color ((sdl:color (random 255) (random 255) (random 255) (random 255)))
	     (sdl-gfx:draw-filledCircle (sdl:point (random width) (random height))
					(random 100))
	     (sdl:draw-rect #(5 65 150 17) :color #(0 0 0))

	     (incf fps-frame-count)
	     (when (> (incf fps-ticks (sdl::get-ticks)) fps-average-window)
	       (setf fps-text (format nil "Current FPS : ~d"
				      (sdl:to-int (coerce (/ fps-frame-count (/ fps-ticks 1000)) 'float))))
	       (setf fps-frame-count 0
		     fps-ticks 0))
	     (sdl-gfx:draw-string #(10 70) fps-text :color #(255 255 255)))
	   (sdl:update-screen)))))))

