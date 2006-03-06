;;;; Demonstration/Test of using the SDL_gfx library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Luke Crook
;;;; see COPYING for license

(in-package :sdl-gfx-examples) 

(defun inbuilt-font ()
  (lispbuilder-sdl::load-sdl-library)
  (sdl-gfx:load-sdl-gfx-library)
  (format t "Starting.....~%")
  (let ((width 640) (height 480))
    (lispbuilder-sdl::with-init ()
      (let ((display (lispbuilder-sdl::set-window width height)))
	(sdl-gfx:gfxPrimitivesSetFont sdl-gfx:gfxPrimitivesFontdata 8 8)
	(sdl-gfx:stringRGBA display 10 70 "Hello World!!!!"  255 255 255 255)
	(lispbuilder-sdl::with-events
	  (:quit t)
	  (:idle
	   (lispbuilder-sdl::update-surface display)))))))
