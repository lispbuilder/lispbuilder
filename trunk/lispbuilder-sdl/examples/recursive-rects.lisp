;; Demonstration/Test of using SDL (Simple Media Layer) library
;; using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones
;; see COPYING for license

;; To run this sample
;; (load "sdl_recursive_rects.lisp")
;; (sdl:recursive-rects) recursively and randomly divides up the screen with rectangles

(in-package #:sdl-examples)
  
; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)

; utilities used in this sample
(defun random0-n(n)
  "get a random number from 0 to n"
  (if (= n 0)
      0
    (random n)))

; recursive rectangle demo
(defun draw-recursive-rects (surface_ptr x1 y1 x2 y2 min-size &optional (level 0))
  "Draw rectangles randomly subdividing the given co-ordinates"
  (let
      ((w (abs (- x1 x2)))
       (h (abs (- y1 y2))))
    (let
	((sx (+ x1 (random0-n w)))
	 (sy (+ y1 (random0-n h))))
      (if (or
	   (>= min-size w)
	   (>= min-size h))
	  (sdl:draw-box (sdl:rectangle-from-edges-* x1 y1 x2 y2) 
			:surface surface_ptr
			:color (sdl:color :r (* 16 (random 16))
					  :g 0
					  :b 0
					  :a 255)
			:clipping nil)
	  (progn
	    (draw-recursive-rects surface_ptr x1 y1 sx sy min-size (1+ level))
	    (draw-recursive-rects surface_ptr sx y1 x2 sy min-size (1+ level))
	    (draw-recursive-rects surface_ptr x1 sy sx y2 min-size (1+ level)) 
	    (draw-recursive-rects surface_ptr sx sy x2 y2 min-size (1+ level)))))))


; main function

(defun recursive-rects()
  "recursively and randomly divides up the screen with rectangles"
  (sdl:with-init ()
    (sdl:window *SCREEN-WIDTH* *SCREEN-HEIGHT*)
    (setf (sdl:frame-rate) 5)
    (draw-recursive-rects sdl:*default-display*
                          0 0 *SCREEN-WIDTH* *SCREEN-HEIGHT* 10)
    (format t "video mode set. width ~a height ~a~%"
            (sdl:width sdl:*default-display*) (sdl:height sdl:*default-display*))
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
       (if (sdl:key= key :SDL-KEY-ESCAPE)
         (sdl:push-quit-event)))
      (:video-expose-event () (sdl:update-display))
      (:idle () (sdl:update-display)))))
