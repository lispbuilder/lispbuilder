;;;;; Converted from the "Functions" Processing example at:
;;;;; "http://www.processing.org/learning/examples/functions.html"
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-gfx-examples) 

(defun draw-target (xloc yloc size num)
  (let ((grayvalues (sdl:cast-to-int (/ 255 num)))
	(steps (sdl:cast-to-int (/ size num))))

    (dotimes (i num)
      (sdl:with-color (col (sdl:color :r (* i grayvalues) :g (* i grayvalues) :b (* i grayvalues)))
	(sdl-gfx:draw-filled-ellipse-* xloc yloc (- size (* i steps)) (- size (* i steps))
				       :surface sdl:*default-display*)))))

(defun functions ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Functions, from Processing.org")
      (setf (sdl:frame-rate) 5)
      (sdl:clear-display (sdl:color :r 51 :g 51 :b 51))
      (draw-target 68 34 100 10)
      (draw-target 152 16 50 3)
      (draw-target 100 144 40 5)

      (sdl:update-display)
      (sdl:with-events ()
		       (:quit-event () t)
		       (:video-expose-event () (sdl:update-display))))))
