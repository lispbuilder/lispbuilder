
(in-package #:lispbuilder-sdl)

(defclass simple-font-definition (bitmap-font-definition)
  ((color-key :accessor color-key :initform nil :initarg :color-key)
   (character-map :accessor character-map :initform nil :initarg :character-map)
   (character-mask :accessor character-mask :initform nil :initarg :character-mask)))

(defmethod initialize-instance :after ((fd simple-font-definition)
                                       &key &allow-other-keys)
  (setf (data fd) (make-array (* (char-size fd) 255)
                              :element-type '(unsigned-byte 8) :initial-element #x0)))
