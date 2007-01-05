;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

;;; Finalize the Surface, freeing SDL_Surface.
(defmethod initialize-instance :after ((self surface) &key)
  (let ((foreign-pointer (fp self)))
    (cffi:finalize self (lambda ()
			  (sdl-cffi::sdl-free-surface foreign-pointer)))))

;;; Finalize the color-struct, freeing SDL_Color
(defmethod initialize-instance :after ((self foreign-color) &key)
  (let ((foreign-pointer (fp self)))
    (cffi:finalize self (lambda ()
			  (cffi:foreign-free foreign-pointer)))))

;;; Finalize the Rectangle, freeing SDL_Rect
(defmethod initialize-instance :after ((self rectangle) &key)
  (let ((foreign-pointer (fp self)))
    (cffi:finalize self (lambda ()
			  (cffi:foreign-free foreign-pointer)))))

(defmethod initialize-instance :after ((self null-rectangle) &key)
  nil)