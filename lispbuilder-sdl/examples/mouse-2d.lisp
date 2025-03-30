;;;;; Converted from the "Mouse 2D" Processing example at:
;;;;; http://www.processing.org/learning/examples/mouse2d.html
;;;;; (C)2007 Luke J Crook

(in-package #:sdl-examples)

(defun mouse-2d ()
  (let ((width 200) (height 200))
    (sdl:with-init ()
      (sdl:window width height :title-caption "Mouse 2D, from Processing.org")
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
	(:quit-event () t)
        (:key-down-event ()
         (when (sdl:key-pressed-p :sdl-key-escape)
           (sdl:push-quit-event)))
	(:mouse-motion-event (:x x :y y)
         (sdl:clear-display sdl:*black*)
         (sdl:with-color (col (sdl:color :r 255 :g 255 :b 255 :a 170))
           (sdl:draw-box (sdl:rectangle-from-midpoint-* x (/ height 2)
                                                        (+ (/ y 2) 10)
                                                        (+ (/ y 2) 10))
                         :alpha 255)
           (sdl:draw-box (sdl:rectangle-from-midpoint-* (- width x) (/ height 2)
                                                        (+ (/ (- height y) 2) 10)
                                                        (+ (/ (- height y) 2) 10))
                         :alpha 255)))
	(:idle () (sdl:update-display))))))

(defun mouse-surface-2d ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "Move a SURFACE using the Mouse.")
    (setf (sdl:frame-rate) 30)

    (sdl:with-surface (movable-surface (sdl:create-surface 20 20))
      (sdl:fill-surface sdl:*white*)

      (sdl:with-events ()
        (:quit-event () t)
        (:mouse-motion-event (:x x :y y)
         ;; Set the texture position with center at the mouse x/y coordinates.
         (sdl:set-surface-* movable-surface
                            :x (- x (/ (sdl:width movable-surface) 2))
                            :y (- y (/ (sdl:height movable-surface) 2))))
        (:key-down-event (:key key)
         (when (eq key :sdl-key-escape)
           (sdl:push-quit-event)))
        (:video-expose-event ()
         (sdl:update-display))
        (:idle ()
         (sdl:clear-display sdl:*black*)
         (sdl:draw-surface movable-surface :surface sdl:*default-display*)
         (sdl:update-display))))))

(defun mouse-rect-2d ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "Move a rectangle using the mouse")
    (setf (sdl:frame-rate) 60)

    (sdl:clear-display sdl:*black* :update t)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (when (eq key :sdl-key-escape)
         (sdl:push-quit-event)))
      (:idle ()
       ;; Clear the display each game loop
       ;;(sdl:clear-display sdl:*black*)

       ;; Draw the box having a center at the mouse x/y coordinates.
       (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
                     :color (if (sdl:mouse-left-p) ;; Change the color of the box if the left mouse button is depressed
                              (sdl:color :r (random 255) :g (random 255) :b (random 255))
                              sdl:*white*))

       ;; Redraw the display
       (sdl:update-display)))))

(defparameter *cursor*
  '(2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    2 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    2 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 
    2 1 1 2 0 0 0 0 0 0 0 1 1 0 0 0
    2 1 1 1 2 0 0 0 0 0 0 1 1 0 0 0 
    2 1 1 1 1 2 0 0 0 0 0 0 0 0 0 0 
    2 1 1 1 1 1 2 0 0 0 0 0 0 0 0 0
    2 1 1 1 2 2 2 0 0 0 0 0 0 0 0 0 
    2 1 1 1 1 2 0 0 0 0 0 0 0 0 0 0 
    2 2 0 2 1 2 2 0 0 0 0 0 0 0 0 0
    2 0 0 0 2 1 2 0 0 0 0 0 0 0 0 0 
    0 0 0 0 2 2 0 0 0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(defun mouse-cursor-and-warp ()
  (sdl:with-init ()
    (sdl:window 200 200 :title-caption "Mouse-cursor")
    (setf (sdl:frame-rate) 60)
    
    ;; Takes any CL sequence of x*x (x must be multiples of 8).
    ;;   1 = white,  2 = black, 3 = inverted, any other value = transparent.
    (sdl:mouse-create-cursor *cursor*)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (cond ((equal key :sdl-key-x)
			      ;; Moves the mouse to the select pixel position relative the window's pixel. Must be a positive integer.
			      (sdl:mouse-warp :x 500))
			     ((equal key :sdl-key-y)
			      (sdl:mouse-warp :y 50))
			     (t (sdl:mouse-warp :x 75 :y 75))))
      (:idle
       (sdl:clear-display sdl:*blue*)
       (sdl:update-display)))))
