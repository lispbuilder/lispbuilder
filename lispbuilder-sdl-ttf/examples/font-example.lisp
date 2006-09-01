;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defvar *ttf-path* (or *load-truename* *default-pathname-defaults*))

(defun font-example ()
  (sdl:with-init ()
    (sdl:with-display (320 240)
      (sdl-ttf:with-open-font ("Vera.ttf" 32 *ttf-path*)
	(sdl-ttf:render-font-solid "Hello TTF World!" :update-p t))
      (sdl:with-events ()
	(:quit () t)
	(:videoexpose () (sdl:update-display))))))
