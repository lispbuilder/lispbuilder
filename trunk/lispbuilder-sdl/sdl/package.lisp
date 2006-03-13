;;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :lispbuilder-sdl
  (:nicknames :sdl)
  (:use :common-lisp :cffi)
  (:documentation "The main package of `lispbuilder-sdl'."))

(in-package :lispbuilder-sdl) 

; sdl library and sdl init helpers
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CHANGE *sdl-binaries-user-path* TO LOCATE YOUR SDL DLL
  ;; Justin TODO Frank had some ideas on how to make this more flexible

  (defparameter *SDL-LOADED* nil)

  ;; First priority, try load the binary from the user-specified location.
#+windows  (defparameter *sdl-binaries-user-path* #P"C:/SDL-1.2.9/lib/SDL")
#+unix  (defparameter *sdl-binaries-user-path* #P"/usr/lib")

  ;; Second priority, try load the binary from the sdl-binaries package-specified location.
  (defparameter *sdl-binaries-default-path* #P"")

  ;; Third priority, try search all directories in the ASDF:*central-registry*
  ;; for the binary and load the first one found.
  (defparameter *sdl-binaries-asdf-path* #P"SDL")

  ;; This parameter stores the location from where the binary was loaded,
  ;; in case the user wants to know.
  (defparameter *sdl-binaries-load-path* nil)

  (defun load-sdl-library()
    "load the sdl library"
    (if *SDL-LOADED*
        (format t "SDL library already loaded~%")
      (progn
	(setf *sdl-binaries-load-path* nil)
	;; Search priority 1
	(if (probe-file (concatenate 'string (namestring *sdl-binaries-user-path*) ".dll"))
	    (setf *sdl-binaries-load-path* *sdl-binaries-user-path*))
	;; Search priority 2
	(unless *sdl-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-binaries-default-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-binaries-load-path* (merge-pathnames *sdl-binaries-default-path* (eval path))))))
	;; Search priority 3
	(unless *sdl-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-binaries-asdf-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-binaries-load-path* (merge-pathnames *sdl-binaries-asdf-path* (eval path))))))
	;;Attempt to load binary.
	(if *sdl-binaries-load-path*
	    (format t "Found \"~A\".... " (concatenate 'string (namestring *sdl-binaries-load-path*) ".dll"))
          (setf *sdl-binaries-load-path* #P"SDL"))
	(format t "attempting to load the SDL runtime.~%")
	(cffi:load-foreign-library *sdl-binaries-load-path*)
	(setf *SDL-LOADED* t)
	(format t "Runtime loaded.~%"))))

#+windows  (load-sdl-library)
#+(and unix cmu) (ext:load-foreign "/usr/lib/libSDL.so")
)

(defun unload-sdl-library()
  "Unload the library when done"
  (if *SDL-LOADED*
      (progn 
	(cffi::close-foreign-library *sdl-binaries-load-path*)
	(format t "Closed the SDL runtime library~%")
	(setf *SDL-LOADED* nil))
      (format t "SDL runtime library is not loaded~%")))

