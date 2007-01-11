;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun inbuilt-font ()
  (sdl:with-init ()
    (sdl:window 640 480)
    (setf (sdl:frame-rate) 5)
    (sdl:clear-display (sdl:color))
    (sdl-gfx:gfx-Primitives-Set-Font sdl-gfx:*font-data* 8 8)
    (sdl-gfx:draw-string "Hello World!!!!" (sdl:point :x 10 :y 70)
			 :color (sdl:color :r 255 :g 255 :b 255)
			 :surface sdl:*default-display*)
    (sdl:with-events ()
		     (:quit-event () t)
		     (:video-expose-event () (sdl:update-display)))))
