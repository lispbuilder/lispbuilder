;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Functions


(defun set-window (width height &key (bpp 0) (flags sdl-cffi::SDL-SW-SURFACE) title-caption icon-caption)
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (let ((surf (sdl-base::set-screen width height
				    :bpp bpp
				    :flags flags
				    :title-caption title-caption
				    :icon-caption icon-caption)))
    (setf *default-display* (surface surf t))))

(defun update-display (&optional (surface *default-display*))
  (sdl-cffi::sdl-flip (fp surface)))

(defun clear-display (color &optional *default-display*)
  (sdl-base::fill-surface (fp *default-display*)
			  (map-color color *default-display*)))