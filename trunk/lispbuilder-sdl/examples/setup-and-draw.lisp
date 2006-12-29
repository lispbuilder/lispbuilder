;;;;; Converted from the "Setup and Draw" Processing example at:
;;;;; "http://www.processing.org/learning/examples/setupdraw.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 

(defun setup-and-draw ()
  (let ((width 200) (height 200)
	(y 100))
    (sdl:with-init ()
      (setf (sdl-base::frame-rate) 30)
      (sdl::window width height :title-caption "Setup and Draw, from Processing.org")
      (sdl:with-events ()
	(:quit-event () t)
	(:idle ()
	       (sdl::clear-display #(0 0 0))
	       (decf y 1)
	       (when (< y 0)
		 (setf y height))
	       (sdl::draw-hline-xy 0 width y
				   :clipping-p nil
				   :color #(255 255 255)
				   :surface sdl::*default-surface*)
		 (sdl:update-display))))))
