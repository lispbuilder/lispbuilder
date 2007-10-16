
(in-package #:lispbuilder-sdl)

(defclass renderer ()
  ((flags :accessor renderer-flags :initform nil)
   (initialized :accessor renderer-initialized :initform nil)
   (width :accessor window-initial-width :initform '() :initarg :window-width)
   (height :accessor window-initial-height :initform '() :initarg :window-height)
   (icon-caption :accessor window-icon-caption :initform '() :initarg :icon-caption)
   (title-caption :accessor window-title-caption :initform '() :initarg :title-caption)
   (bpp :accessor window-bpp :initform nil :initarg :bpp)
   (surface :accessor window-surface :initform nil :initarg :surface)))

(defclass sdl (renderer)
  ((flags :accessor renderer-flags :initform sdl-cffi::SDL-INIT-VIDEO)))

(defclass opengl (renderer) ())
