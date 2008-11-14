
(in-package #:sdl-gfx-examples)

(defun every-n-frames (max)
  (let ((count 0))
    #'(lambda ()
	(if (eql 0 (mod (incf count 1) max))
	    (setf count 0)
	    nil))))

(defun draw-fps (string x y font surface render-p)
  ;; Create a new FPS string when render-p is T
  (when render-p
    (sdl:render-string-shaded string sdl:*white* sdl:*black* :font font :cache t :free t))
  ;; Draw the string each frame
  (sdl:draw-font-at-* x y :font font :surface surface))
