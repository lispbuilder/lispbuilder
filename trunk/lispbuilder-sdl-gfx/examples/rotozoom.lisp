
(in-package #:sdl-gfx-examples)

(defvar rotozoom-window-width 320)
(defvar rotozoom-window-height 240)

(defun rotozoom ()
  (sdl:with-init ()
    (sdl:window rotozoom-window-width rotozoom-window-height
		:title-caption "Rotozoom" :icon-caption "Rotozoom")

    ;; Unlock the frame rate.
    (setf (sdl:frame-rate) 60)

    ;; Load the image to be rotated.
    (let* ((alien-image (sdl:load-image (sdl:create-path "lisp.bmp" *bmp-path*)
				  :key-color-at (sdl:point :x 0 :y 0)))
	   ;; Pre-render the rotated images.
	   (rotos (loop
		     for angle from 360 above 0 by 5
		     collecting (sdl-gfx:roto-zoom-surface (coerce angle 'double-float)
							   1.0d0
							   1 :surface alien-image))))
      
      (let ((rotated-image rotos))
	
	(sdl:with-events ()
	  (:quit-event () t)
	  (:idle ()
		 (sdl:clear-display sdl:*black*)

		 ;; Move through the list of pre-rendered images.
		 (setf rotated-image (cdr rotated-image))
		 (unless rotated-image
		   (setf rotated-image rotos))

		 ;; Render the rotated image around the center of the surface.
		 (let ((image (car rotated-image)))
		   (sdl:draw-surface-at-* image
					  (sdl:cast-to-int (- (/ rotozoom-window-width 2)
							      (/ (sdl:width image) 2)))
					  (sdl:cast-to-int (- (/ rotozoom-window-height 2)
							      (/ (sdl:height image) 2))))
		   
		   (sdl:update-display))))))))