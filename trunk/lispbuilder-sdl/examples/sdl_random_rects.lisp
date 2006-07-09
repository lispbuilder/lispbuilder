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
      (sdl:with-display (width height
			       :flags sdl:SDL_SWSURFACE
			       :title-caption "Random-rects1"
			       :icon-caption "Random-rects1")
	(sdl:with-events ()
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (eq key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   ;;Update only the portion of the display that has been written to.
	   (sdl:with-default-color ((sdl:random-color))
	     (sdl:draw-rect (sdl:random-rect width height) :update-p t))))))))

(defun random-rects2 ()
  (sdl:with-init ()			; Initialize Systems
    (sdl:set-framerate 0)		; Unlock the framerate.
    (sdl:with-display (320 240)
      (sdl:with-events ()
	(:quit t)
	(:keydown (state scancode key mod unicode)
		  (if (eq key :SDLK_ESCAPE)
		      (sdl:push-quitevent)))
	(:idle
	 (sdl:draw-rect (sdl:random-rect (sdl:surf-w) (sdl:surf-h)) :color (sdl:random-color)
			:update-p nil)
	 ;; Update the entire display.
	 (sdl:update-display))))))

(defun random-rects3 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()			; Initialize Systems
      (sdl:set-framerate 0)		; Unlock the framerate.
      (sdl:with-display (320 240 :surface-name display)
	(sdl:with-events ()
	  (:quit t)
	  (:keydown (state scancode key mod unicode)
		    (if (eq key :SDLK_ESCAPE)
			(sdl:push-quitevent)))
	  (:idle
	   (sdl:draw-rect-end-points (+ 1 (random width)) (+ 1 (random height))
				     (+ 1 (random width)) (+ 1 (random height))
				     :color (sdl:random-color) :surface display :clipping-p t :update-p t)))))))

