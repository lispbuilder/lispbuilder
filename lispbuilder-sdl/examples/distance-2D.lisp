;;;;; Converted from the "Distance 2D" Processing example at:
;;;;; "http://www.processing.org/learning/examples/distance2d.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 


(defun distance-2D ()
  (let ((width 200) (height 200) (mouse-x 0) (mouse-y 0)
	(max-distance (sdl:distance 0 0 200 200)))
    (sdl:with-init ()
      (sdl:with-display (width height :title-caption "Distance 2D, from Processing.org")
	(sdl:set-framerate 30)
	(sdl:with-events ()
	  (:quit () t)
	  (:mousemotion (:x x :y y)
			(setf mouse-x x
			      mouse-y y))
	  (:idle ()
		 (sdl:clear-display :color #(0 0 0))
		 (sdl:with-color (#(255 255 255))
		   (loop for i from 20 below width by 20
		      do (loop for j from 20 below height by 20
			    do (let ((size (* (/ (sdl:distance mouse-x mouse-y i j)
						 max-distance)
					      66)))
				 (sdl:draw-box :rectangle (sdl:rect-from-midpoint i j size size))))))
		 (sdl:update-display)))))))
