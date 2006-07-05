;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defvar *circles-per-frame* 1)
(defvar *fps-average-window* 500)

(let ((fps-ticks 0) (fps-frame-count 0) (fps-text ""))
  (defun display-fps (surface)
    (sdl:draw-rect #(5 65 150 17) :color #(0 0 0) :surface surface)   
    (incf fps-frame-count)
    (when (> (incf fps-ticks (sdl::get-ticks)) *fps-average-window*)
      (setf fps-text (format nil "Current FPS : ~d" (sdl:to-int (coerce (/ fps-frame-count
									   (/ fps-ticks 1000))
									'float))))
      (setf fps-frame-count 0
	    fps-ticks 0))
    (sdl-gfx:draw-string fps-text :position #(10 70) :surface surface :color #(255 255 255))))

(defun random-circles ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl-gfx:gfxPrimitivesSetFont sdl-gfx:font-data 8 8)
      (let ((width (sdl:surf-w sdl:*default-display*))
	    (height (sdl:surf-h sdl:*default-display*)))
	(sdl:set-framerate 0)
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (dotimes (i *circles-per-frame*)
	     (sdl-gfx:draw-filledCircle (random 100)
					:position (sdl:point (random width) (random height))
					:color (sdl:color (random 255) (random 255) (random 255) (random 255))))
	   (display-fps sdl:*default-surface*)
	   (sdl:update-screen)))))))
