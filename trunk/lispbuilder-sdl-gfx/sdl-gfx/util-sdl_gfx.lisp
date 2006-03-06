;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package :lispbuilder-sdl-gfx) 
  
(defparameter *SDL-GFX-LOADED* nil)

; CHANGE THIS TO LOCATE YOUR SDL_GFX DLL
; Justin TODO Frank had some ideas on how to make this more flexible
(defparameter *sdl-gfx-dll-filepath* "C:\\SDL_gfx-2.0.13\\lib\\SDL_gfx")


; sdl library and sdl init helpers
(defun load-sdl-gfx-library()
  "load the sdl library"
  (if *SDL-GFX-LOADED*
      (format t "SDL_gfx runtime already loaded~%")
      (progn 
	(format t "Loading SDL_gfx runtime~%")
	(cffi:load-foreign-library *sdl-gfx-dll-filepath*)
	(setf *SDL-GFX-LOADED* t))))

(defun unload-sdl-gfx-library()
  "Unload the library when done"
  (if *SDL-GFX-LOADED*
      (progn 
	(cffi::close-foreign-library *sdl-gfx-dll-filepath*)
	(format t "Closed SDL_gfx runtime library~%")
	(setf *SDL-GFX-LOADED* nil))
      (format t "SDL_gfx runtime library is not loaded~%")))

;; (defun load-font (path width height)
;;   (if path
      
;;       )

;;   )
