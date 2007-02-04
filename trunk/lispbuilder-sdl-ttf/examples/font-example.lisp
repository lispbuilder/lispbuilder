;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defun font-example ()
  (sdl:with-init ()
    (sdl:window 320 300)
    (sdl-ttf:initialise-default-font)
    (sdl:with-surface (disp sdl:*default-display*)
      (sdl-ttf:draw-string-solid-* "Text LATIN1 - Solid" 0 0
				   :color (sdl:color :r 255 :g 0 :b 255)
				   :encoding :latin1)
      (sdl-ttf:draw-string-solid-* "Text UTF8 - Solid" 0 50
				   :color (sdl:color :r 255 :g 0 :b 255)
				   :encoding :utf8)
      (sdl-ttf:draw-string-shaded-* "Text LATIN1 - Shaded" 0 100
				    (sdl:color :r 255 :g 0 :b 255) (sdl:color :r 255 :g 255 :b 255)
				   :encoding :latin1)
      (sdl-ttf:draw-string-shaded-* "Text UTF8 - Shaded" 0 150
				    (sdl:color :r 255 :g 0 :b 255) (sdl:color :r 255 :g 255 :b 255)
				    :encoding :latin1)
      (sdl-ttf:draw-string-blended-* "Text LATIN1 - Blended" 0 200
				     :color (sdl:color :r 255 :g 0 :b 255)
				     :encoding :latin1)
      (sdl-ttf:draw-string-blended-* "Text UTF8 - Blended" 0 250
				     :color (sdl:color :r 255 :g 0 :b 255)
				     :encoding :latin1))
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display)))))


