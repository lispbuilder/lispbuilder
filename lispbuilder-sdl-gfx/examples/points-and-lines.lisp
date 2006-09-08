;;;;; Converted from the "Points and Lines" Processing example at:
;;;;; "http://www.processing.org/learning/examples/pointslines.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun points-and-lines ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (*width* *height* :title-caption "Points and Lines, from Processing.org")
	(sdl:set-framerate 10)
	(sdl:clear-display :color #(0 0 0))
	(let* ((d 40) (p1 d) (p2 (+ p1 d)) (p3 (+ p2 d)) (p4 (+ p3 d)))
	  (sdl-gfx:draw-rectangle (sdl:rectangle p2 p2 d d) :color #(153 153 153))
	  (sdl:with-color (#(255 255 255))
	    (mapcar #'(lambda (point)
			(sdl-gfx:draw-pixel :position point))
		    (list (sdl:point p1 p1) (sdl:point p1 p3) (sdl:point p2 p4)
			  (sdl:point p3 p1) (sdl:point p4 p2) (sdl:point p4 p4)))))
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))
