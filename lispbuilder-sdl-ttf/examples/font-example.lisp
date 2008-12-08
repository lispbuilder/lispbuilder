;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defun font-example ()
  (sdl:with-init ()
    (sdl:window 320 300 :title-caption "SDL-TTF Font Example"
                :icon-caption "SDL-TTF Font Example")
    (setf (sdl:frame-rate) 5)
    (sdl:fill-surface sdl:*white* :surface sdl:*default-display*)
    (unless (sdl:initialise-default-font sdl:*ttf-font-vera*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))
    (sdl:draw-string-solid-* "Text UTF8 - Solid" 0 50
                             :color sdl:*black*)
    (sdl:draw-string-shaded-* "Text UTF8 - Shaded" 0 150
                              sdl:*black*
                              sdl:*yellow*)
    (sdl:draw-string-blended-* "Text UTF8 - Blended" 0 250
                               :color sdl:*black*)
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:idle () (sdl:update-display)))))
