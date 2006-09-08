;;;;; Converted from the "Functions" Processing example at:
;;;;; "http://www.processing.org/learning/examples/functions.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun draw-target (xloc yloc size num)
  (let ((grayvalues (sdl:to-int (/ 255 num)))
	(steps (sdl:to-int (/ size num))))
    
    (dotimes (i num)
      (sdl-gfx:draw-filledellipse (- size (* i steps)) (- size (* i steps))
				  :position (sdl:point xloc yloc)
				  :color (sdl:color (* i grayvalues)
						    (* i grayvalues)
						    (* i grayvalues))))))

(defun functions ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:with-display (width height :title-caption "Functions, from Processing.org")
	(sdl:clear-display :color #(51 51 51))
	(draw-target 68 34 100 10)
	(draw-target 152 16 50 3)
	(draw-target 100 144 40 5)
	(sdl:with-events ()
	  (:quit () t)
	  (:videoexpose () (sdl:update-display)))))))
