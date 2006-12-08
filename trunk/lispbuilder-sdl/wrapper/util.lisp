
(in-package #:lispbuilder-sdl-base)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(declaim (inline to-int))
(defun to-int (num)
  (truncate (+ 0.5 num)))

(defun vec-to-int (vec)
  "vec-to-int will create a new VECTOR of the same length as VEC, but the contents are converted to integers.
   Returns VEC if the contents are not of type float."
  (if (vectorp vec)
      (let ((require-conversion nil)
	    (length (length vec)))
	(block convert
	  (dotimes (i length)
	    (when (floatp (svref vec i))
	      (setf require-conversion t)
	      (return-from convert))))
	(if require-conversion
	    (let ((new-vec (make-array (length vec) :initial-element 0)))
	      (dotimes (i length)
		(setf (svref new-vec i) (to-int (svref vec i))))
	      new-vec)
	    vec))
      nil))
