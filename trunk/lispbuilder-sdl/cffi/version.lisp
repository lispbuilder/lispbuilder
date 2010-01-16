
(in-package #:lispbuilder-sdl-cffi) 

(defconstant SDL-MAJOR-VERSION 1)
(defconstant SDL-MINOR-VERSION 2)
(defconstant SDL-PATCH-LEVEL 14)

(cffi:defcstruct SDL-version
  (major :unsigned-char)
  (minor :unsigned-char)
  (patch :unsigned-char))

(cffi:defcfun ("SDL_Linked_Version" SDL-Linked-Version) :pointer)

(defun set-sdl-version (x)
  (cffi:with-foreign-slots ((major minor patch) x SDL-version)
    (setf major SDL-MAJOR-VERSION
          minor SDL-MINOR-VERSION
          patch SDL-PATCH-LEVEL)))

(defun version-number (major minor patch)
  (+  (* major 1000)
      (* minor 100)
      patch))

(defun version-at-least (version x y z)
  (if (>= version (version-number x y z))
      t
      nil))

(defun get-library-version (version)
  (cffi:with-foreign-slots ((major minor patch) version sdl-version)
    (version-number major minor patch)))

(defun sdl-library-version ()
  (get-library-version (sdl-linked-version)))

(defun sdl-version-at-least (x y z)
  (when (>= (sdl-library-version) (version-number x y z))
      t))

(defun sdl-wrapper-version ()
  (version-number sdl-major-version sdl-minor-version sdl-patch-level))
