;;;;; Converted from the "Vertices" Processing example at:
;;;;; "http://www.processing.org/learning/examples/vertices.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 


(defun vertices ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Vertices, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color))
      (sdl:with-color (a-col (sdl:color :r 102 :g 102 :b 102))
	;; We can use a point or an x/y spread as the vertex parameter
	;; Here we use x/y spread as the vertex parameters
	(sdl:with-curve (:solid 10)
	  (sdl:add-vertex-* 168 182)
	  (sdl:add-vertex-* 168 182)
	  (sdl:add-vertex-* 136 38)
	  (sdl:add-vertex-* 42  34)
	  (sdl:add-vertex-* 64  200)
	  (sdl:add-vertex-* 64  200)))
      (sdl:with-color (a-col (sdl:color :r 51 :g 51 :b 51))
	;; Here we use a point as the vertex parameter
	(sdl:with-shape (:dash)
	  (sdl:add-vertex (sdl:point :x 60 :y 40))
	  (sdl:add-vertex (sdl:point :x 160 :y 10))
	  (sdl:add-vertex (sdl:point :x 170 :y 150))
	  (sdl:add-vertex (sdl:point :x 60 :y 150))))
      (sdl:with-color (a-col (sdl:color :r 255 :g 255 :b 255))
	;; Here we use x/y spread as the vertex parameters
	(sdl:with-shape (:points)
	  (sdl:add-vertex-* 60  40)
	  (sdl:add-vertex-* 160 10)
	  (sdl:add-vertex-* 60  150)
	  (sdl:add-vertex-* 170 150)))
      (sdl:with-color (a-col (sdl:color :r 126 :g 126 :b 126))
	;; Here we use a point as the vertex parameter
	(sdl:with-bezier (:solid)
	  (sdl:add-vertex (sdl:point :x 60 :y 40))
	  (sdl:add-vertex (sdl:point :x 160 :y 10))
	  (sdl:add-vertex (sdl:point :x 170 :y 150))
	  (sdl:add-vertex (sdl:point :x 60 :y 150))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))


