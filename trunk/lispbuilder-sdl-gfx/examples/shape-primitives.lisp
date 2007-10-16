;;;;; Converted from the "Shape Primitives" Processing example at:
;;;;; "http://www.processing.org/learning/examples/shapeprimitives.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun shape-primitives ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Shape Primitives, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color))
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (col (sdl:color :r 226 :g 226 :b 226))
	  (sdl-gfx:draw-filled-trigon (sdl:point :x 10 :y 10)
				      (sdl:point :x 10 :y 200)
				      (sdl:point :x 45 :y 200))
	  (sdl-gfx:draw-box (sdl:rectangle :x 45 :y 45 :w 35 :h 35))
	  (sdl-gfx:draw-filled-polygon (list (sdl:point :x 105 :y 10)
					     (sdl:point :x 120 :y 10)
					     (sdl:point :x 120 :y 200)
					     (sdl:point :x 80 :y 200)))
	  (sdl-gfx:draw-filled-ellipse (sdl:point :x 140 :y 80)
				       (/ 40 2) (/ 40 2))
	  (sdl-gfx:draw-filled-trigon (sdl:point :x 160 :y 10)
				      (sdl:point :x 195 :y 200)
				      (sdl:point :x 160 :y 200))))

      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))


