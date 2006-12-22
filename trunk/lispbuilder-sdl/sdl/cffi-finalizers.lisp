;; lispbuilder-sdl-oo
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

;;; Finalize the Surface, freeing SDL_Surface.
(defmethod initialize-instance :after ((self surface) &key)
  (let ((foreign-pointer (fp self)))
    (setf (slot-value self 'foreign-pointer-to-surface) nil
	  (slot-value self 'pixel-reader) nil
	  (slot-value self 'pixel-writer) nil
	  (slot-value self 'width) 0
	  (slot-value self 'height) 0)
    (finalize self (lambda ()
		     (sdl-cffi::sdl-free-surface foreign-pointer)
		     (cffi:foreign-free foreign-pointer)))))

;;; Finalize the color-struct, freeing SDL_Color
(defmethod initialize-instance :after ((self color-struct) &key)
  (let ((foreign-pointer (fp self)))
    (finalize self (lambda ()
		     (cffi:foreign-free foreign-pointer)))))

;;; Finalize the Rectangle, freeing SDL_Rect
(defmethod initialize-instance :after ((self rectangle) &key)
  (let ((foreign-pointer (fp self)))
    (finalize self (lambda ()
		     (cffi:foreign-free foreign-pointer)))))
