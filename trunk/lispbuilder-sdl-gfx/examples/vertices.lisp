;;;;; Converted from the "Vertices" Processing example at:
;;;;; "http://www.processing.org/learning/examples/vertices.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 


(defun vertices ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Vertices, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color))
      (sdl:with-surface (disp sdl:*default-display*)
	(sdl:with-color (col (sdl:color :r 102 :g 102 :b 102))
	  ;; We can use a point or an x/y spread as the vertex parameter
	  ;; Here we use x/y spread as the vertex parameters
	  (sdl-gfx:with-curve (:solid 10)
	    (sdl-gfx:add-vertex-* 168 182)
	    (sdl-gfx:add-vertex-* 168 182)
	    (sdl-gfx:add-vertex-* 136 38)
	    (sdl-gfx:add-vertex-* 42 34)
	    (sdl-gfx:add-vertex-* 64 200)
	    (sdl-gfx:add-vertex-* 64 200)))
	(sdl:with-color (col (sdl:color :r 51 :g 51 :b 51))
	  ;; Here we use a point as the vertex parameter
	  (sdl-gfx:with-shape (:dash)
	    (sdl-gfx:add-vertex (sdl:point :x 60 :y 40))
	    (sdl-gfx:add-vertex (sdl:point :x 160 :y 10))
	    (sdl-gfx:add-vertex (sdl:point :x 170 :y 150))
	    (sdl-gfx:add-vertex (sdl:point :x 60 :y 150))))
	(sdl:with-color (col (sdl:color :r 255 :g 255 :b 255))
	  ;; Here we use x/y spread as the vertex parameters
	  (sdl-gfx:with-shape (:points)
	    (sdl-gfx:add-vertex-* 60 40)
	    (sdl-gfx:add-vertex-* 160 10)
	    (sdl-gfx:add-vertex-* 170 150)
	    (sdl-gfx:add-vertex-* 60 150)))
	(sdl:with-color (col (sdl:color :r 126 :g 126 :b 126))
	  ;; Here we use a point as the vertex parameter
	  (sdl-gfx:with-bezier (10)
	    (sdl-gfx:add-vertex (sdl:point :x 60 :y 40))
	    (sdl-gfx:add-vertex (sdl:point :x 160 :y 10))
	    (sdl-gfx:add-vertex (sdl:point :x 170 :y 150))
	    (sdl-gfx:add-vertex (sdl:point :x 60 :y 150)))))

      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))

