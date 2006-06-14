;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

;;;; To run these samples
;;;; (sdl:random-rects1)
;;;; (sdl:random-rects2)
;;;; (sdl:random-rects3)

(in-package #:sdl-examples) 
  
(defun random-rects1 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()			; Initialize Systems
      (sdl:set-framerate 0)		; Unlock the framerate.
      (sdl::with-surface ((sdl:set-window width height
					  :flags sdl:SDL_SWSURFACE
					  :title-caption "Random-rects1"
					  :icon-caption "Random-rects1")
			  :surface-name display)
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   ;;Update only the portion of the display that has been written to.
	   (sdl:draw-rect display (sdl::random-rect width height) (sdl::random-color) :update-p t)))))))

(defun random-rects2 ()
  (sdl:with-init ()			; Initialize Systems
    (sdl:set-framerate 0)		; Unlock the framerate.
    (sdl::with-display (320 240)
      (sdl:with-events
	(:quit t)
	(:keydown (state scancode key mod unicode)
		  (if (sdl:is-key key :SDLK_ESCAPE)
		      (sdl:push-quitevent)))
	(:idle
	 (sdl:draw-rect sdl:*display
			(sdl::random-rect (sdl:surf-w sdl:*display) (sdl:surf-h sdl:*display))
			(sdl::random-color)
			:update-p nil)
	 ;;Update only entire display.
	 (sdl:update-screen sdl:*display))))))

(defun random-rects3 ()
  (let ((width 640) (height 480))
    (sdl:with-init ()			; Initialize Systems
      (sdl:set-framerate 0)		; Unlock the framerate.
      (let ((display (sdl:set-window width height
				     :flags '(sdl:SDL_ANYFORMAT sdl:SDL_SWSURFACE)
				     :title-caption "Random-rects3"
				     :icon-caption "Random-rects3")))
	(sdl:with-events
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (sdl:is-key key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:draw-rect-end-points display
				     (+ 1 (random width)) (+ 1 (random height))
				     (+ 1 (random width)) (+ 1 (random height))
				     (sdl::random-color) :clipping-p t :update-p t)))))))

