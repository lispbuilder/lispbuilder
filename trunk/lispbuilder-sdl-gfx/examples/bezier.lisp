;;;;; Converted from the "Bezier" Processing example at:
;;;;; "http://www.processing.org/learning/examples/bezier.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun bezier ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (width height :title-caption "Bezier, from Processing.org")
	(sdl:set-framerate 5)
	(sdl:clear-display :color #(0 0 0))
	(sdl:with-color (#(255 255 255))
	  (loop for i from 0 to 100 by 10
	       do (sdl-gfx:draw-bezier (list (sdl:point (- 90 (/ i 2.0)) (+ 20 i))
					     (sdl:point 210 10)
					     (sdl:point 220 150)
					     (sdl:point (- 120 (/ i 8.0)) (+ 150 (/ i 4.0))))
				       100)))
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))
