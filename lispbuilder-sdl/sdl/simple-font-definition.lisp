
(in-package #:lispbuilder-sdl)

(defclass simple-font-definition (bitmap-font-definition)
  ((color-key :accessor color-key :initform nil :initarg :color-key)
   (character-map :accessor character-map :initform nil :initarg :character-map)
   (pad-x :accessor pad-x :initform 0 :initarg :pad-x)
   (pad-y :accessor pad-y :initform 0 :initarg :pad-y)))

(defmethod initialize-instance :after ((fd simple-font-definition)
                                       &key &allow-other-keys)
  (setf (data fd) (make-array (* (char-size fd) 256)
                              :initial-element #x00))
  (unless (consp (character-map fd))
    (setf (character-map fd) (list (character-map fd)))))
