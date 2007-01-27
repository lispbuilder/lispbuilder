
(in-package #:lispbuilder-sdl-ttf)

(defclass font ()
  ((foreign-pointer-to-font :accessor fp-font :initform nil :initarg :font)
   (font-style :accessor font-style :initform nil :initarg :style)
   (font-encoding :accessor font-encoding :initform nil :initarg :encoding)
   (cached-surface :accessor cached-surface :initform nil)))

(defun new-font (fp)
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

(defun free-font (font)
  "Free's the resources used by FONT."
  (sdl-ttf-cffi::ttf-close-font (fp-font font))
  #-clisp(cffi:cancel-finalization font)
  )