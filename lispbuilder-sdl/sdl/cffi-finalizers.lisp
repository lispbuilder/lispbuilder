;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

;;; Finalize the Surface, freeing SDL_Surface.
(defmethod initialize-instance :after ((self surface) &key)
  "Free the foreign SDL_Surface and the SDL_Rect used for position.
Cannot free the SDL_Rect used as the cell mask as this is set
after the SURFACE is created."
  (let ((foreign-pointer (fp self))
	(fp-position (fp-position self)))
    (tg:finalize self (lambda ()
			(sdl-cffi::sdl-free-surface foreign-pointer)
			(cffi:foreign-free fp-position)))))

;;; Finalize the color-struct, freeing SDL_Color
(defmethod initialize-instance :after ((self foreign-color) &key)
  (let ((foreign-pointer (fp self)))
    (tg:finalize self (lambda ()
			  (cffi:foreign-free foreign-pointer)))))

;;; Finalize the Rectangle, freeing SDL_Rect
(defmethod initialize-instance :after ((self rectangle) &key)
  (let ((foreign-pointer (fp self)))
    (tg:finalize self (lambda ()
			  (cffi:foreign-free foreign-pointer)))))

(defmethod initialize-instance :after ((self null-rectangle) &key)
  nil)

;;; Finalize the Rwops, freeing SDL_RWOps
(defmethod initialize-instance :after ((self rwops) &key)
  (let ((foreign-pointer (fp self)))
    (tg:finalize self (lambda ()
			  (sdl-cffi::SDL-Free-RW foreign-pointer)))))

;;; Finalize the FONT, freeing all associated surfaces
(defmethod initialize-instance :after ((self bitmap-font) &key)
  (let ((font-data (font-data self))
	(hash-table (characters self)))
    (tg:finalize self (lambda ()
			(cffi:foreign-free font-data)
;;  			#-clisp(maphash #'(lambda (key val)
;; 					    (declare (ignore key))
;; 					    (free-surface (glyph-surface val)))
;; 					hash-table)
			(clrhash hash-table)))))
