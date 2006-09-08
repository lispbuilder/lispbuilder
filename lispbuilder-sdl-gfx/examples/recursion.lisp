;;;;; Converted from the "Recursion" Processing example at:
;;;;; "http://www.processing.org/learning/examples/recursion.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun draw-circle (x radius level)
  (let ((tt (* 126 (/ level 4.0))))
    (sdl-gfx:draw-filledellipse (sdl:to-int radius) (sdl:to-int radius)
				:position (sdl:point x 100)
				:color (sdl:color tt tt tt))
    (when (> level 1)
      (decf level 1)
      (draw-circle (- x (/ radius 2)) (/ radius 2) level)
      (draw-circle (+ x (/ radius 2)) (/ radius 2) level))))

(defun recursion ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (width height :title-caption "Recursion, from Processing.org")
	(sdl:clear-display :color #(0 0 0))
	(draw-circle 126 170 6)
	(sdl:update-display)
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))
