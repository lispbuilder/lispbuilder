;;; -*- lisp -*-

(in-package #:lispbuilder-sdl-cffi) 

;; cffi:foreign-library-loaded-p is not yet in the released version of CFFI
;;(defparameter *image-loaded-p* (cffi:foreign-library-loaded-p 'sdl-image))
(defparameter *image-loaded-p* nil)

(cffi:define-foreign-library sdl-image
    (:darwin (:framework "SDL_image"))
    (:windows (:or "SDL_image.dll" "SDL_image1.2.dll"))
    (:unix (:or "libSDL_image-1.2.so.0"
            "libSDL_image1.2" 
            "libSDL_image.so"))
    (t (:or)))

(cffi:define-foreign-library zlib
  (:windows (:or "zlib1.dll")))
(cffi:define-foreign-library libpng
  (:windows (:or "libpng12-0.dll")))
(cffi:define-foreign-library libjpg
  (:windows (:or "jpeg.dll")))
(cffi:define-foreign-library libtiff
  (:windows (:or "libtiff-3.dll")))

(defun load-image-library ()
  (when (handler-case (cffi:use-foreign-library sdl-image)
          (load-foreign-library-error () nil))
    (progn
      (setf *image-loaded-p* t)
      (pushnew :lispbuilder-sdl-image *features*)))

  (when (handler-case (cffi:use-foreign-library zlib)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library libpng)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library libjpg)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library libtiff)
          (load-foreign-library-error () nil))))

(eval-when (:load-toplevel :execute)
  (load-image-library))

