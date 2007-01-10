;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-examples) 
  
(defun random-rects-1 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (setf (sdl-base::frame-rate) 0)
      (sdl:window 320 240 :title-caption "random-rects-1" :icon-caption "random-rects-1")
      (sdl:with-events ()
		       (:quit-event () t)
		       (:key-down-event (:key key)
					(if (sdl-base::key= key :SDL-KEY-ESCAPE)
					    (sdl-base::push-quit-event)))
		       (:idle ()
			      (sdl:fill-surface (sdl:color :r (random 255)
							   :g (random 255)
							   :b (random 255))
						:template (sdl:random-rectangle width height (sdl:rectangle))
						:surface sdl:*default-display*
						:update-p t))))))

(defun random-rects-2 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (setf (sdl-base::frame-rate) 0)
      (sdl:window 320 240 :title-caption "random-rects-2" :icon-caption "random-rects-2")
      (sdl:with-events ()
		       (:quit-event () t)
		       (:idle ()
			      (sdl:with-rectangle (rect (sdl:random-rectangle width height))
				(sdl:with-color (col (sdl:color))
				  (sdl:set-color-* col :r (random 255) :g (random 255) :b (random 255))
				  (sdl:fill-surface col
						    :template rect
						    :surface sdl:*default-display*
						    :update-p t))))))))

(defun random-rects-3 ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (setf (sdl-base::frame-rate) 0)
      (sdl:window 320 240 :title-caption "random-rects-3" :icon-caption "random-rects-3")
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl:draw-box (sdl:random-rectangle width height)
			     :color (sdl:color :r (random 255) :g (random 255) :b (random 255))
			     :surface sdl:*default-display*)
	       (sdl:update-display))))))
