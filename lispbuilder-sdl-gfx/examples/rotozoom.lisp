
(in-package #:sdl-gfx-examples)

(defvar rotozoom-window-width 320)
(defvar rotozoom-window-height 240)

(defun rotozoom ()
  (sdl:with-init ()
    (sdl:window rotozoom-window-width rotozoom-window-height
		:title-caption "Rotozoom" :icon-caption "Rotozoom")

    ;; Lock the frame rate to 60 fps.
    ;; Don't want the image to rotate too fast.
    (setf (sdl:frame-rate) 60)

    ;; Load the image to be rotated.
    (let* ((alien-image (sdl:load-image (sdl:create-path "lisp.bmp" *bmp-path*)
					:color-key-at (sdl:point :x 0 :y 0)))
	   ;; Pre-render the rotated images.
	   (rotos (loop
                   for angle from 360 above 0 by 5
                   collecting (sdl-gfx:rotate-surface angle :smooth t :surface alien-image))))
      
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

(defun zoom ()
  (sdl:with-init ()
    (sdl:window rotozoom-window-width rotozoom-window-height
		:title-caption "Zoom" :icon-caption "Zoom")

    (setf (sdl:frame-rate) 30)
    (sdl:enable-key-repeat nil nil)

    ;; Load the image to be rotated.
    (let* ((alien-image (sdl:load-image (sdl:create-path "lisp.bmp" *bmp-path*)
					:color-key-at (sdl:point :x 0 :y 0)))
           (zoom-x 1.0) (zoom-y 1.0))
                 
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
         (cond
          ((sdl:key= key :sdl-key-q)
           (incf zoom-x 0.1))
          ((sdl:key= key :sdl-key-w)
           (decf zoom-x 0.1))
          ((sdl:key= key :sdl-key-a)
           (incf zoom-y 0.1))
          ((sdl:key= key :sdl-key-s)
           (decf zoom-y 0.1))))
        (:idle ()
         (sdl:clear-display sdl:*black*)
         (sdl:draw-surface-at-* (sdl:convert-to-display-format
                                 :surface (sdl-gfx:zoom-surface
                                           zoom-x zoom-y
                                           :surface alien-image)
                                 :inherit t)
                                 (sdl:cast-to-int (- (/ rotozoom-window-width 2)
                                                     (/ (sdl:width alien-image) 2)))
                                 (sdl:cast-to-int (- (/ rotozoom-window-height 2)
                                                     (/ (sdl:height alien-image) 2))))
                                (sdl:update-display))))))

