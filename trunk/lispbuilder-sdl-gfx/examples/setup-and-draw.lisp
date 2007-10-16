;;;;; Converted from the "Setup and Draw" Processing example at:
;;;;; "http://www.processing.org/learning/examples/setupdraw.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun setup-and-draw ()
  (let ((width 200) (height 200)
	(y 100))
    (sdl:with-init ()
      (setf (sdl:frame-rate) 30)
      (sdl:window width height :title-caption "Setup and Draw, from Processing.org")
      (sdl:with-events ()
		       (:quit-event () t)
		       (:idle ()
			      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
			      (decf y 1)
			      (when (< y 0)
				(setf y height))
			      (sdl-gfx:draw-hline 0 width y
						  :color (sdl:color :r 255 :g 255 :b 255)
						  :surface sdl:*default-display*)
			      (sdl:update-display))))))
