;;;;; Converted from the "CL-VECTORS" tutorial at:
;;;;; "http://projects.tuxee.net/cl-vectors/section-tutorial#tutorial"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 

(defun cl-vectors-1 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "CL-VECTORS, Tutorial #1")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))

      (let ((state (aa:make-state)))       ; create the state
	(aa:line-f state 200 50 250 150)   ; describe the 3 sides
	(aa:line-f state 250 150 50 100)   ; of the triangle
	(aa:line-f state 50 100 200 50)
	(let ((put-pixel (sdl:image-put-pixel sdl:*default-display* #(0 0 0))))
	  (aa:cells-sweep state put-pixel) ; render it
	  (sdl:with-events ()
	    (:quit-event () t)
	    (:video-expose-event () (sdl:update-display))
	    (:idle () (sdl:update-display))))))))

(defun cl-vectors-2 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "CL-VECTORS, Tutorial #2")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
      
      (let ((state (aa:make-state)))	; create the state
	;; the 1st triangle
	(aa:line-f state 200 50 250 150) ; describe the 3 sides
	(aa:line-f state 250 150 50 100) ; of the first triangle
	(aa:line-f state 50 100 200 50)
	;; the 2nd triangle
	(aa:line-f state 75 25 10 75)	; describe the 3 sides
	(aa:line-f state 10 75 175 100)	; of the second triangle
	(aa:line-f state 175 100 75 25)
	(let ((put-pixel (sdl:image-put-pixel sdl:*default-display* #(0 0 0))))
	  (aa:cells-sweep state put-pixel) ; render it
	  
	  (sdl:with-events ()
	    (:quit-event () t)
	    (:video-expose-event () (sdl:update-display))
	    (:idle () (sdl:update-display))))))))

(defun cl-vectors-3 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "CL-VECTORS, Tutorial #3")
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
	(aa:cells-sweep state1 (sdl:image-put-pixel sdl:*default-display* #(255 0 0)))
	(aa:cells-sweep state2 (sdl:image-put-pixel sdl:*default-display* #(0 0 255)))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:idle () (sdl:update-display)))))))

(defun cl-vectors-4 ()
  (let ((width 300) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "CL-VECTORS, Rendering a Glyph")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))

      (zpb-ttf:with-font-loader (loader sdl:*default-ttf-font*)
	(aa:cells-sweep (vectors:update-state (aa:make-state)
					      (paths-ttf:paths-from-glyph (zpb-ttf:find-glyph #\A loader)
									  :offset (paths:make-point 50 160)
									  :scale-x 0.1
									  :scale-y -0.1))
			(sdl:image-put-pixel sdl:*default-display* #(255 0 0)))
	(sdl:with-events ()
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
	  (:idle () (sdl:update-display)))))))

