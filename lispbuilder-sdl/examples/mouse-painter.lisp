;; lispbuilder-sdl sample program
;; (C)2006 Frank Buss
;; see COPYING for license
;; From "http://www.frank-buss.de/lisp/canvas.html"

(in-package #:sdl-examples)

(defun mouse-painter ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Mouse Painter" :icon-caption "Mouse Painter")
    (setf (sdl:frame-rate) 30)
    (sdl:clear-display (sdl:color :r 255 :g 255 :b 255) :update t)

    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event ()
       (cond
        ((sdl:key-pressed-p :SDL-KEY-ESCAPE)
         (sdl:push-quit-event))
        ((sdl:key-pressed-p :SDL-KEY-SPACE)
         (sdl:clear-display (sdl:color :r 255 :g 255 :b 255))
         (sdl:update-display))))
      (:mouse-motion-event (:x x :y y :x-rel xrel :y-rel yrel)
       (when (sdl:mouse-left-p)
         (sdl:draw-line-* x y (- x xrel) (- y yrel) :color (sdl:color))
         (sdl:update-display))))))
