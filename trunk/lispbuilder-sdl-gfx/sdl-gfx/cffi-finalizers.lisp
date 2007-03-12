;; lispbuilder-sdl
;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl-gfx)

;;; Finalize the FONT, freeing all associated surfaces
(defmethod initialize-instance :after ((self font) &key)
  (let ((font-data (font-data self)))
    (tg:finalize self (lambda ()
			(cffi:foreign-free font-data)))))
