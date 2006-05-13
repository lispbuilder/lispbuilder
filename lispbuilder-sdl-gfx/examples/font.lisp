;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package #:sdl-gfx-examples) 

(defun inbuilt-font ()
  (let ((width 640) (height 480))
    (sdl:with-init ()
      (let ((display (sdl:set-window width height)))
	(sdl-gfx:gfxPrimitivesSetFont sdl-gfx:gfxPrimitivesFontdata 8 8)
	(sdl-gfx:stringRGBA display 10 70 "Hello World!!!!"  255 255 255 255)
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl:update-surface display)))))))
