;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)



(defmacro with-display ((surface-name width height
				      &key (flags sdl-cffi::SDL-SW-SURFACE) (bpp 0)
				      (title-caption nil) (icon-caption nil))
			&body body)
  `(with-surface (,surface-name (set-window ,width ,height
					    :bpp ,bpp :flags ,flags
					    :title-caption ,title-caption :icon-caption ,icon-caption)
				nil)
     (if (is-valid-ptr ,surface-name)
	 (progn ,@body))))



(defun set-screen (width height
		   &key (bpp 0) (flags '(sdl-cffi::SDL-HW-SURFACE sdl-cffi::SDL-FULL-SCREEN sdl-cffi::SDL-HW-ACCEL))
		   title-caption icon-caption)
  "Will attempt to create a full screen, hardware accelerated window using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (let ((surface (sdl-cffi::SDL-Set-Video-Mode width height bpp (set-flags flags))))
    (when (is-valid-ptr surface)
      (when (or title-caption icon-caption) 
	(sdl-cffi::sdl-WM-Set-Caption title-caption icon-caption))
      surface)))

(defun set-window (width height &key (bpp 0) (flags sdl-cffi::SDL-SW-SURFACE) title-caption icon-caption)
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (set-screen width height :bpp bpp :flags flags :title-caption title-caption :icon-caption icon-caption))

(defun update-display (surface)
  (sdl-cffi::sdl-flip surface))

;; cl-sdl "cl-sdl.lisp"
(defun clear-display (surface color)
  (fill-surface surface color :clipping nil)
  surface)

(defun video-pixel-format ()
  (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::vfmt))

