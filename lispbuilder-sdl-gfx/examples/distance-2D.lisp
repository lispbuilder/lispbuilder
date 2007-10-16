;;;;; Converted from the "Distance 2D" Processing example at:
;;;;; "http://www.processing.org/learning/examples/distance2d.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun distance-2D ()
  (let ((width 200) (height 200) (mouse-x 0) (mouse-y 0)
	(max-distance (sdl:distance-* 0 0 200 200)))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Distance 2D, from Processing.org")
      (setf (sdl:frame-rate) 30)
      (sdl:with-events ()
		       (:quit-event () t)
		       (:mouse-motion-event (:x x :y y)
					    (setf mouse-x x
						  mouse-y y))
		       (:idle ()
			      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
			      (sdl:with-surface (disp sdl:*default-display*)
				(sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
				  (loop for i from 20 below width by 20
				     do (loop for j from 20 below height by 20
					   do (let ((size (* (/ (sdl:distance-* mouse-x mouse-y i j)
								max-distance)
							     66)))
						(sdl-gfx:draw-box (sdl:rectangle-from-midpoint-* i j size size))))))
				(sdl:update-display)))))))
