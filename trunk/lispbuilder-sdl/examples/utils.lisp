
(in-package #:lispbuilder-sdl-examples)

(let* ((frame-values 60)
       (frame-times (make-array frame-values :initial-element 0 :element-type 'fixnum))
       (frame-time-last 0)
       (frame-count 0))
  (declare (type fixnum frame-values frame-time-last frame-count))

  (defun fps-init ()
    (dotimes (i frame-values)
      (setf (aref frame-times i) 0))
    (setf frame-count 0
	  frame-time-last (sdl-cffi::SDL-get-ticks)))

  (defun display-fps (x y surface)
    (declare (optimize (safety 0) (speed 3) (space 1)))
    (let ((get-ticks (sdl-cffi::SDL-get-ticks))
          (frames-per-second 0.0))
      (declare (type fixnum get-ticks)
	       (type float frames-per-second))
      (setf (aref frame-times frame-count) (- get-ticks frame-time-last))
      (setf frame-time-last get-ticks)
      (incf frame-count)
      (when (>= frame-count frame-values)
	(setf frame-count 0)
	(dotimes (i frame-values)
	  (incf frames-per-second (aref frame-times i)))
	(setf frames-per-second (sdl:cast float (/ 1000 (/ frames-per-second frame-values))))
	(sdl:render-string-solid (format nil "fps : ~2$" (coerce frames-per-second 'float))
				 :color sdl:*white*
				 :cache t
				 :free t))
     (sdl:draw-font-at-* x y :surface surface))))