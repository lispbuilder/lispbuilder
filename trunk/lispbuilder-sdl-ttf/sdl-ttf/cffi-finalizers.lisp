;; lispbuilder-sdl-ttf
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl-ttf)

;;; Finalize the Font, closing and freeing TTF_Font.
(defmethod initialize-instance :after ((self font) &key)
  (let ((foreign-pointer (fp-font self)))
    (tg:finalize self (lambda ()
			(when (is-init)
			  (sdl-ttf-cffi::ttf-close-font foreign-pointer))))))

