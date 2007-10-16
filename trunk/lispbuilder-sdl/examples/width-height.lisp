;;;;; Converted from the "Width and Height" Processing example at:
;;;;; "http://www.processing.org/learning/examples/widthheight.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples) 

(defun width-height ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Width and Height, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 127 :g 127 :b 127))
      
      (loop for i from 0 to height by 20
	 do (progn (sdl:draw-box (sdl:rectangle :x 0 :y i :w 200 :h 10)
				  :color (sdl:color))
		   (sdl:draw-box (sdl:rectangle :x i :y 0 :w 10 :h 200)
				  :color (sdl:color :r 255 :g 255 :b 255))))
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))))))
  
