
(in-package #:lispbuilder-sdl)

(defclass font-definition ()
  ((filename :accessor filename :initform nil :initarg :filename)
   (loader :accessor loader :initform #'load-image :initarg :loader)
   (char-size :reader char-size :initform nil :initarg :size)))
