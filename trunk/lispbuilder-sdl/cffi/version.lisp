
(in-package #:lispbuilder-sdl-cffi) 

;;;; Implementation of SDL macros follows

(cl:defconstant SDL-MAJOR-VERSION 1)

(cl:defconstant SDL-MINOR-VERSION 2)

(cl:defconstant SDL-PATCH-LEVEL 13)

(cffi:defcstruct SDL-version
	(major :unsigned-char)
	(minor :unsigned-char)
	(patch :unsigned-char))

(cffi:defcfun ("SDL_Linked_Version" SDL-Linked-Version) :pointer)



(defun SDL-VERSION (x)
  (cffi:with-foreign-slots ((major minor patch) x SDL-version)
    (setf major SDL-MAJOR-VERSION
          minor SDL-MINOR-VERSION
          patch SDL-PATCH-LEVEL)))

(defun SDL-VERSION-NUM (major minor patch)
        (+  (* major 1000)
            (* minor 100)
            patch))

(defun SDL-COMPILED-VERSION ()
  "Returns the version number of the SDL dynamic library."
        (SDL-VERSION-NUM SDL-MAJOR-VERSION SDL-MINOR-VERSION SDL-PATCH-LEVEL))

(defun SDL-VERSION-AT-LEAST (x y z)
  (if (>= (SDL-COMPILED-VERSION) (SDL-VERSION-NUM x y z))
      1
      0))

