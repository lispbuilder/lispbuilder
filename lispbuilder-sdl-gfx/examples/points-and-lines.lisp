;;;;; Converted from the "Points and Lines" Processing example at:
;;;;; "http://www.processing.org/learning/examples/pointslines.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun points-and-lines ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Points and Lines, from Processing.org")
      (setf (sdl:frame-rate) 5)

      (sdl:clear-display (sdl:color :r 51 :g 51 :b 51))
      (let* ((d 40) (p1 d) (p2 (+ p1 d)) (p3 (+ p2 d)) (p4 (+ p3 d)))
	(sdl-gfx:draw-rectangle (sdl:rectangle :x p2 :y p2 :w d :h d)
				:color (sdl:color :r 153 :g 153 :b 153)
				:surface sdl:*default-display*)
	(mapcar #'(lambda (point)
		    (sdl-gfx:draw-pixel point :surface sdl:*default-display* :color (sdl:color :r 255 :g 255 :b 255)))
		(list (sdl:point :x p1 :y p1) (sdl:point :x p1 :y p3) (sdl:point :x p2 :y p4)
		      (sdl:point :x p3 :y p1) (sdl:point :x p4 :y p2) (sdl:point :x p4 :y p4))))

      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))
