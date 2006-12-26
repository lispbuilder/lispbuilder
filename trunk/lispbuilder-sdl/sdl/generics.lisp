;; The OO wrapper for the lispbuilder-sdl package
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-sdl)


(defgeneric r (color))
(defgeneric g (color))
(defgeneric b (color))
(defgeneric a (color))

(defgeneric (setf r) (value color))
(defgeneric (setf g) (value color))
(defgeneric (setf b) (value color))
(defgeneric (setf a) (value color))

(defgeneric map-color (color &optional surface))
(defgeneric free-color (color))


(defgeneric width (obj)
  (:documentation "Returns the width of the surface or rectangle."))

(defgeneric height (obj)
  (:documentation "Returns the height of the surface or rectangle."))

(defgeneric x (obj)
  (:documentation "Returns the x component of the rectangle."))

(defgeneric y (obj)
  (:documentation "Returns the y component of the rectangle."))

(defgeneric (setf width) (value obj))
(defgeneric (setf height) (value obj))
(defgeneric (setf x) (value obj))
(defgeneric (setf y) (value obj))

(defgeneric x2 (obj))

(defgeneric y2 (obj))

(defgeneric (setf x2) (value obj))
(defgeneric (setf y2) (value obj))



(defgeneric free-surface (surface)
  (:documentation "Free's foreign surface in SURFACE."))




(defgeneric pixel-reader (surface))
(defgeneric pixel-writer (surface))

