;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric fp (object)
  (:documentation "Returns the default foreign object for OBJECT."))

(defgeneric fp-position (object)
  (:documentation "Returns the default SDL_Rect foreign object for OBJECT."))



(defgeneric pack-color (color))

(defgeneric r (color))
(defgeneric g (color))
(defgeneric b (color))
(defgeneric a (color))

(defgeneric (setf r) (value color))
(defgeneric (setf g) (value color))
(defgeneric (setf b) (value color))
(defgeneric (setf a) (value color))

(defgeneric map-color (color &optional surface))


;; set-'s and get-'s
(defgeneric color-* (obj))
(defgeneric set-color (dst src))
(defgeneric set-color-* (obj &key r g b a))

(defgeneric point-* (obj))
(defgeneric set-point (obj point))
(defgeneric set-point-* (obj &key x y))

(defgeneric position-* (obj))
(defgeneric set-position (dst src))
(defgeneric set-position-* (obj &key x y))

(defgeneric rectangle-* (rectangle))
(defgeneric set-rectangle (dst src))
(defgeneric set-rectangle-* (rectangle &key x y w h))

(defgeneric set-surface (dst position))
(defgeneric set-surface-* (surface &key x y))

;; end set-'s and get-'s


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


(defgeneric (setf width) (value obj))
(defgeneric (setf height) (value obj))

(defgeneric (setf x) (value obj))
(defgeneric (setf y) (value obj))

(defgeneric (setf x2) (value obj))
(defgeneric (setf y2) (value obj))

(defgeneric free-color (color))

(defgeneric free-surface (surface)
  (:documentation "Free's foreign SDL_Surface in SURFACE."))

(defgeneric free-rectangle (rectangle)
  (:documentation "Free's foreign SDL_Rect in RECTANGLE."))

(defgeneric free-rwops (rwops)
  (:documentation "Free's foreign SDL_rwops in RWOPS."))

