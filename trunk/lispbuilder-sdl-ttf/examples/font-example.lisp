;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)


(defun font-example ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl-ttf::initialise-default-font)
    (sdl:with-surface (disp sdl:*default-display*)
      (sdl-ttf::draw-string-solid-* "Hello TTF World!" 25 50
				    :color (sdl:color :r 255 :g 0 :b 255)))
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))


