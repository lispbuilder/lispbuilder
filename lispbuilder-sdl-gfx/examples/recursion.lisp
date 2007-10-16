;;;;; Converted from the "Recursion" Processing example at:
;;;;; "http://www.processing.org/learning/examples/recursion.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun draw-circle (x radius level)
  (let ((tt (* 126 (/ level 4.0))))
    (sdl-gfx:draw-filled-ellipse (sdl:point :x x :y 100) (sdl:cast-to-int radius) (sdl:cast-to-int radius)
				 :color (sdl:color :r tt :g tt :b tt)
				 :surface sdl:*default-display*)
    (when (> level 1)
      (decf level 1)
      (draw-circle (- x (/ radius 2)) (/ radius 2) level)
      (draw-circle (+ x (/ radius 2)) (/ radius 2) level))))

(defun recursion ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Recursion, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
      (draw-circle 126 170 6)

      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))
