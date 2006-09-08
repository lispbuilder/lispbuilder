;;;;; Converted from the "Width and Height" Processing example at:
;;;;; "http://www.processing.org/learning/examples/widthheight.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun width-height ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (width height :title-caption "Width and Height, from Processing.org")
	(sdl:clear-display :color #(127 127 127))
	(loop for i from 0 to height by 20
	   do (progn (sdl-gfx:draw-box (sdl:rectangle 0 i 200 10) :color #(0 0 0))
		     (sdl-gfx:draw-box (sdl:rectangle i 0 10 200) :color #(255 255 255))))
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))
  
