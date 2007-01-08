
(in-package #:lispbuilder-sdl-image)

(defgeneric create-image-from-RWops (rwops &key image-type force free))

(defgeneric image-type-of (rwops image-type))

(defgeneric image-type-p (filename pathname))

