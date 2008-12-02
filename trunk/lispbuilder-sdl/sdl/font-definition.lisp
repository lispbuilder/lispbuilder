
(in-package #:lispbuilder-sdl)

(defclass font-definition ()
  ((filename :accessor filename :initform nil :initarg :filename)
   (char-size :reader char-size :initform nil :initarg :size)))
