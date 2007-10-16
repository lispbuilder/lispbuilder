;;;;; Converted from the "Mouse 2D" Processing example at:
;;;;; http://www.processing.org/learning/examples/mouse2d.html
;;;;; (C)2007 Luke J Crook

(in-package #:sdl-examples)

(defun mouse-2d ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (setf (sdl:frame-rate) 60)
      (sdl:window width height :title-caption "Mouse 2D, from Processing.org")
      (sdl:with-events ()
	(:quit-event () t)
	(:mouse-motion-event (:x x :y y)
			     (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255 :a 170))
			       (sdl:clear-display sdl:*black*)
			       (sdl:draw-box (sdl:rectangle-from-midpoint-* x (/ height 2) (+ (/ y 2) 10) (+ (/ y 2) 10))
					     :alpha 255)
			       (sdl:draw-box (sdl:rectangle-from-midpoint-* (- width x) (/ height 2) (+ (/ (- height y) 2) 10) (+ (/ (- height y) 2) 10))
					     :alpha 255)))
	(:idle () (sdl:update-display))))))

