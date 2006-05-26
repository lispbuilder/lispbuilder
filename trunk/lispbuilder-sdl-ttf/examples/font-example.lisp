;;;; Demonstration/Test of using the SDL_ttf library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Rune Nesheim
;;;; see COPYING for license

(in-package #:sdl-ttf-examples)

(defvar *sdl-ttf* (merge-pathnames "Vera.ttf" (or *load-truename* *default-pathname-defaults*)))

(defun font-example ()
  (let ((width 320) (height 240))
    (sdl:with-init ()
      (let ((display (sdl:set-window width height)))
	(sdl-ttf:with-init
	  (let ((font (sdl-ttf:open-font (namestring *sdl-ttf*) 32)))
	    (unwind-protect
	      (let ((text-surf (sdl-ttf:make-text-surface font "Hello TTF World!" #(255 255 255))))
		(sdl:apply-surface text-surf display))
	      (sdl-ttf:close-font font))))
	(sdl:with-events
	  (:quit t)
	  (:idle
	   (sdl:update-surface display)))))))
