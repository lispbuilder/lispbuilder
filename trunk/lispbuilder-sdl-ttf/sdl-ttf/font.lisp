
(in-package #:lispbuilder-sdl-ttf)

(defclass font ()
  ((foreign-pointer-to-font :accessor fp :initform nil :initarg :font)
   (foreign-pointer-to-position-rect :accessor fp-position
				     :initform (cffi:foreign-alloc 'sdl-cffi::sdl-rectangle)
				     :initarg :position)
   (cached-surface :accessor cached-surface :initform nil)))

(defun font (fp)
  (when (sdl:is-valid-ptr fp)
    (make-instance 'font :font fp)))

(defmethod x ((font font))
  (sdl-base::rect-x (fp-position (cached-surface font))))
(defmethod (setf x) (x-val (font font))
  (setf (sdl-base::rect-x (fp-position (cached-surface font))) x-val))

(defmethod y ((font font))
  (sdl-base::rect-y (fp-position (cached-surface font))))
(defmethod (setf y) (y-val (font font))
  (setf (sdl-base::rect-y (fp-position (cached-surface font))) y-val))

(defmethod fp-position ((font font))
  (fp-position (cached-surface font)))

(defmethod free-font ((font font))
  (sdl-ttf-cffi::ttf-close-font (fp font))
  #-clisp(cffi:cancel-finalization rectangle)
  )
