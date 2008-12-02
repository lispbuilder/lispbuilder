
(in-package #:lispbuilder-sdl)

(defclass bitmap-font-definition (font-definition)
  ((char-width :accessor char-width :initform nil :initarg :width)
   (char-height :accessor char-height :initform nil :initarg :height)
   (char-pitch :reader char-pitch :initform nil :initarg :pitch)
   (data :accessor data :initform nil :initarg :data)))

(defmethod initialize-instance :after ((fd bitmap-font-definition)
                                       &key &allow-other-keys)
  (setf (slot-value fd 'char-pitch) (truncate (/ (+ (char-width fd) 7) 8)))
  (setf (slot-value fd 'char-size) (* (char-pitch fd) (char-height fd))))
