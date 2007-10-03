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


(defun cl-vectors-1 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Vertices, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))

      (let ((state (aa:make-state)))       ; create the state
	(aa:line-f state 200 50 250 150)   ; describe the 3 sides
	(aa:line-f state 250 150 50 100)   ; of the triangle
	(aa:line-f state 50 100 200 50)
	(let* ((put-pixel (sdl::image-put-pixel sdl:*default-display* #(0 0 0))))
	  (aa:cells-sweep state put-pixel) ; render it
	  (sdl:with-events ()
	    (:quit-event () t)
	    (:video-expose-event () (sdl:update-display))
	    (:idle () (sdl:update-display))))))))


(defun cl-vectors-2 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Vertices, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))

;;; Note that the image size is 4x3. Zoom it when it is displayed.
      (let* ((put-pixel (sdl::image-put-pixel sdl:*default-display* #(0 0 0))))
	(funcall put-pixel 2 1 256)  ; full opacity
	(funcall put-pixel 1 2 128)  ; half opacity
	(funcall put-pixel 3 2 192)  ; 3/4 opacity
	(funcall put-pixel 0 1 0)    ; null opacity, nothing drawn
	
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:idle () (sdl:update-display)))))))


(defun cl-vectors-3 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Vertices, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
      
      (let ((state1 (aa:make-state))
	    (state2 (aa:make-state)))
	;; the 1st triangle
	(aa:line-f state1 200 50 250 150)   ; describe the 3 sides
	(aa:line-f state1 250 150 50 100)   ; of the first triangle
	(aa:line-f state1 50 100 200 50)
	;; the 2nd triangle
	(aa:line-f state2 75 25 10 75)      ; describe the 3 sides
	(aa:line-f state2 10 75 175 100)    ; of the second triangle
	(aa:line-f state2 175 100 75 25)
	(aa:cells-sweep state1 (sdl::image-put-pixel sdl:*default-display* #(255 0 0)))
	(aa:cells-sweep state2 (sdl::image-put-pixel sdl:*default-display* #(0 0 255)))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:idle () (sdl:update-display)))))))

