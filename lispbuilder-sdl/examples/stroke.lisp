;;;;; Converted from the "Pattern" Processing example at:
;;;;; http://www.processing.org/learning/examples/pattern.html
;;;;; (C)2006 Luke J Crook

(in-package #:sdl-examples)

(defun stroke ()
  (let ((prev-mouse-x 0) (prev-mouse-y 0))
    (sdl:with-init ()
      (sdl:window 200 200
                  :title-caption "Stroke, from Processing."
                  :icon-caption "Stroke, from Processing.")
      (setf (sdl:frame-rate) 60)
      (sdl:clear-display (sdl:color :r 102 :g 102 :b 102))
      (sdl:with-events ()
	(:quit-event () t)
	(:video-expose-event () (sdl:update-display))
	(:idle () (sdl:update-display))
	(:mouse-motion-event (:x x :y y)
         (let ((speed (sdl-base::clamp (+ (abs (- x prev-mouse-x))
                                          (abs (- y prev-mouse-y)))
                                       0 255)))
           (sdl:draw-filled-circle-* x y speed
                                     :color sdl:*white*
                                     :stroke-color sdl:*black*)
           (setf prev-mouse-x x
                 prev-mouse-y y)))))))

