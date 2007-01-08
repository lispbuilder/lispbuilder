;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric fp (object)
  (:documentation "Returns the default foreign object for OBJECT."))

(defgeneric fp-position (object)
  (:documentation "Returns the default SDL_Rect foreign object for OBJECT."))



(defgeneric r (color))
(defgeneric g (color))
(defgeneric b (color))
(defgeneric a (color))

(defgeneric (setf r) (value color))
(defgeneric (setf g) (value color))
(defgeneric (setf b) (value color))
(defgeneric (setf a) (value color))

(defgeneric map-color (color &optional surface))

(defgeneric set-color (obj &key r g b a))

(defgeneric width (obj)
  (:documentation "Returns the width of the surface or rectangle."))

(defgeneric height (obj)
  (:documentation "Returns the height of the surface or rectangle."))

(defgeneric x (obj)
  (:documentation "Returns the x component of the rectangle."))

(defgeneric y (obj)
  (:documentation "Returns the y component of the rectangle."))

(defgeneric x2 (obj))
(defgeneric y2 (obj))

(defgeneric xy (obj))
(defgeneric set-xy (obj x y))

(defgeneric (setf width) (value obj))
(defgeneric (setf height) (value obj))

(defgeneric (setf x) (value obj))
(defgeneric (setf y) (value obj))

(defgeneric (setf x2) (value obj))
(defgeneric (setf y2) (value obj))

(defgeneric (setf pos) (value obj))

(defgeneric point-from (obj))

(defgeneric free-color (color))

(defgeneric free-surface (surface)
  (:documentation "Free's foreign SDL_Surface in SURFACE."))

(defgeneric free-rectangle (rectangle)
  (:documentation "Free's foreign SDL_Rect in RECTANGLE."))

(defgeneric free-rwops (rwops)
  (:documentation "Free's foreign SDL_rwops in RWOPS."))
