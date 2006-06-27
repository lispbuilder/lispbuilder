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
	    (height (sdl:surf-h sdl:*default-display*)))
	(sdl:set-framerate 60)
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:with-default-color ((sdl:color (random 255) (random 255) (random 255) (random 255)))
	     (sdl-gfx:draw-filledCircle (sdl:point (random width) (random height))
					(random 100))
	     (sdl:draw-rect #(5 65 180 17) :color #(0 0 0))
	     (sdl-gfx:draw-string #(10 70) (format nil "Current FPS : ~d" (sdl:to-int (coerce (if (< 0 (sdl::get-ticks))
											      (/ 1000 (sdl::get-ticks))
											      0) 'float)))				  :color #(255 255 255)))
	   (sdl:update-screen)))))))

