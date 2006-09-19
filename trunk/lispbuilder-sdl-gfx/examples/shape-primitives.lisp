;;;;; Converted from the "Shape Primitives" Processing example at:
;;;;; "http://www.processing.org/learning/examples/shapeprimitives.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun shape-primitives ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (width height
			 :title-caption "Shape Primitives, from Processing.org")
	(sdl:set-framerate 5)
	(sdl:clear-display :color #(0 0 0))
	(sdl:with-color (#(226 226 226))
	  (sdl-gfx:draw-filledtrigon (sdl:point 10 10)
				     (sdl:point 10 200)
				     (sdl:point 45 200))
	  (sdl-gfx:draw-box (sdl:rectangle 45 45 35 35))
 	  (sdl-gfx:draw-filledpolygon (list (sdl:point 105 10)
					    (sdl:point 120 10)
					    (sdl:point 120 200)
					    (sdl:point 80 200)))
	  (sdl-gfx:draw-filledellipse (/ 40 2) (/ 40 2)
				      :position #(140 80))
	  (sdl-gfx:draw-filledtrigon (sdl:point 160 10)
				     (sdl:point 195 200)
				     (sdl:point 160 200)))
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))


