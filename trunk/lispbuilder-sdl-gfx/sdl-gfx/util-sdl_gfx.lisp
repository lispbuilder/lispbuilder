;; SDL_gfx v2.0.13 library. Uses CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>, Justin Heyes-Jones <justinhj@gmail.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL_gfx from Common lisp
;; using sdl_gfx.lisp (the CFFI wrapper)

(in-package :lispbuilder-sdl-gfx)
  
(defparameter *SDL-GFX-LOADED* nil)

; CHANGE *sdl-gfx-binaries-user-path* TO LOCATE YOUR SDL_GFX DLL
; Justin TODO Frank had some ideas on how to make this more flexible

; First priority, try load the binary from the user-specified location.
(defparameter *sdl-gfx-binaries-user-path* #P"C:/SDL_gfx-2.0.13/lib/SDL_gfx")

; Second priority, try load the binary from the sdl-gfx-binaries package-specified location.
(defparameter *sdl-gfx-binaries-default-path* #P"")

; Third priority, try search all directories in the ASDF:*central-registry*
; for the binary and load the first one found.
(defparameter *sdl-gfx-binaries-asdf-path* #P"SDL_gfx")

; This parameter stores the location from where the binary was loaded,
; in case the user wants to know.
(defparameter *sdl-gfx-binaries-load-path* nil)

; sdl library and sdl init helpers
(defun load-sdl-gfx-library()
  "load the sdl library"
  (if *SDL-GFX-LOADED*
      (format t "SDL_gfx runtime already loaded~%")
      (progn
	(setf *sdl-gfx-binaries-load-path* nil)
	;; Search priority 1
	(if (probe-file (concatenate 'string (namestring *sdl-gfx-binaries-user-path*) ".dll"))
	    (setf *sdl-gfx-binaries-load-path* *sdl-gfx-binaries-user-path*))
	;; Search priority 2
	(unless *sdl-gfx-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-gfx-binaries-default-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-gfx-binaries-load-path* (merge-pathnames *sdl-gfx-binaries-default-path* (eval path))))))
	;; Search priority 3
	(unless *sdl-gfx-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-gfx-binaries-asdf-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-gfx-binaries-load-path* (merge-pathnames *sdl-gfx-binaries-asdf-path* (eval path))))))
	;;Attempt to load binary.
	(if *sdl-gfx-binaries-load-path*
	    (format t "Found \"~A\".... " (concatenate 'string (namestring *sdl-gfx-binaries-load-path*) ".dll")))
	(format t "attempting to load SDL_gfx runtime.~%")
	(cffi:load-foreign-library *sdl-gfx-binaries-load-path*)
	(setf *SDL-GFX-LOADED* t)
	(format t "Runtime loaded.~%"))))

(defun unload-sdl-gfx-library()
  "Unload the library when done"
  (if *SDL-GFX-LOADED*
      (progn 
	(cffi::close-foreign-library *sdl-gfx-binaries-load-path*)
	(format t "Closed SDL_gfx runtime library~%")
	(setf *SDL-GFX-LOADED* nil))
      (format t "SDL_gfx runtime library is not loaded~%")))

;; (defun load-font (path width height)
;;   (if path
      
;;       )

;;   )
