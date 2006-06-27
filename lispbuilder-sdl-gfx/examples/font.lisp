;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun inbuilt-font ()
  (sdl:with-init ()
    (sdl:with-display (640 480)
      (sdl-gfx:gfxPrimitivesSetFont sdl-gfx:font-data 8 8)
      (sdl-gfx:draw-string #(10 70) "Hello World!!!!" :color #(255 255 255))
      (sdl:with-events
	(:quit t)
	(:idle
	 (sdl:update-screen))))))
