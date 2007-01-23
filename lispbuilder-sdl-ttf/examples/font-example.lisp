;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defvar *ttf-path* (or *load-truename* *default-pathname-defaults*))

(defun font-example ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl-ttf:initialize-default-font "Vera.ttf" 32 *ttf-path*)
    (sdl-ttf:render-font-solid "Hello TTF World!")
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))


