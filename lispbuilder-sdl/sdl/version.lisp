
(in-package #:lispbuilder-sdl)

(defun version-number (major minor patch)
  (+  (* major 1000)
      (* minor 100)
      patch))

(defun library-version (version)
  (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) version sdl-cffi::sdl-version)
    (version-number sdl-cffi::major sdl-cffi::minor sdl-cffi::patch)))

(defun sdl-library-version ()
  (library-version (sdl-cffi::sdl-linked-version)))

(defun sdl-glue-version ()
  (version-number sdl-cffi::sdl-major-version sdl-cffi::sdl-minor-version sdl-cffi::sdl-patch-level))

(defun version-at-least (version x y z)
  (if (>= version (version-number x y z))
      t
      nil))
