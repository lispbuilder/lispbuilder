
(in-package #:lispbuilder-sdl-examples)

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

;; (let* ((frame-values 60)
;;        (frame-times (make-array frame-values :initial-element 0 :element-type 'fixnum))
;;        (frame-time-last 0)
;;        (frame-count 0))
;;   (declare (type fixnum frame-values frame-time-last frame-count))

;;   (defun fps-init ()
;;     (dotimes (i frame-values)
;;       (setf (aref frame-times i) 0))
;;     (setf frame-count 0
;; 	  frame-time-last (the fixnum (sdl-cffi::SDL-get-ticks))))

;;   (defun display-fps (x y surface)
;;     (declare (optimize (safety 0) (speed 3))
;; 	     (type fixnum x y))
;;     (let ((get-ticks (the fixnum (sdl-cffi::SDL-get-ticks)))
;;           (frames-per-second 0.0))
;;       (declare (type fixnum get-ticks)
;; 	       (type single-float frames-per-second))
;;       (setf (aref frame-times frame-count) (the fixnum (- get-ticks frame-time-last)))
;;       (setf frame-time-last get-ticks)
;;       (incf frame-count)
;;       (when (>= frame-count frame-values)
;; 	(setf frame-count 0)
;; 	(dotimes (i frame-values)
;; 	  (incf frames-per-second (aref frame-times i)))
;; 	(setf frames-per-second (the single-float (/ 1000 (the single-float (/ frames-per-second frame-values)))))
;; 	(sdl:render-string-solid (format nil "FPS : ~2$" frames-per-second)
;; 				 :color sdl:*white*
;; 				 :cache t
;; 				 :free t))
;;      (sdl:draw-font-at-* x y :surface surface))))