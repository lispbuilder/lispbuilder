;;;;; Converted from the "Distance 2D" Processing example at:
;;;;; "http://www.processing.org/learning/examples/distance2d.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

(defun rect-at-midpoint (x y w h)
  (sdl:rectangle (- x (/ w 2))
		 (- y (/ h 2))
		 w h))

(defun distance-2D ()
  (let ((width 200) (height 200) (mouse-x 0) (mouse-y 0)
	(max-distance (distance 0 0 200 200)))
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
			    do (let ((size (* (/ (distance mouse-x mouse-y i j)
						 max-distance)
					      66)))
				 (sdl-gfx:draw-box (rect-at-midpoint i j size size))))))
		 (sdl:update-display)))))))
