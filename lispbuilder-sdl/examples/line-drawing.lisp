;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Frank Busse
;;;; see COPYING for license

;;;; From "http://www.frank-buss.de/lisp/canvas.html"

(in-package #:sdl-examples) 

(defun line-drawing ()
  (sdl:with-init ()
    (sdl:window 300 300 :title-caption "Line Drawing" :icon-caption "Line Drawing")
    (setf (sdl:frame-rate) 5)
    (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
    
    (sdl:with-surface (surf sdl:*default-display*)
      (sdl:draw-box (sdl:rectangle-from-edges-* 50 50 250 250)
		    :color (sdl:color :r 0 :g 255 :b 255))
      (sdl:with-color (col (sdl:color :r 0 :g 0 :b 0))
	(loop for i from 50 to 250 by 5
              do (sdl:draw-line-* (- 300 i) 50 50 i))
	(sdl:draw-line-* 250 250 250 50)
	(sdl:draw-line-* 250 250 50 250))
      (sdl:update-display)      
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
         (if (sdl:key= key :SDL-KEY-ESCAPE)
           (sdl:push-quit-event)))
	(:video-expose-event () (sdl:update-display))))))
