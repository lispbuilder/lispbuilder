;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)



;;; w
(defmacro with-init (init-flags &body body)
  "Attempts to initialize the SDL subsystems using SDL-Init.
   Automatically shuts down the SDL subsystems using SDL-Quit upon normal application termination or
   if any fatal error occurs within &body.
   init-flags can be any combination of SDL-INIT-TIMER, SDL-INIT-AUDIO, SDL-INIT-VIDEO, SDL-INIT-CDROM,
   SDL-INIT-JOYSTICK, SDL-INIT-NOPARACHUTE, SDL-INIT-EVENTTHREAD or SDL-INIT-EVERYTHING."
  `(block nil
    (unwind-protect
	 (when (init-sdl ,@(when init-flags
				 `(:flags (list ,@init-flags))))
	   ,@body)
      (sdl-cffi::SDL-Quit))))

(defun init-sdl (&key (flags nil))
  (if (equal 0 (sdl-cffi::SDL-Init (set-flags flags)))
      t
      nil))

(defun init-p (&key (flags))
  (if (equal (set-flags flags)
	     (sdl-cffi::sdl-was-init (set-flags flags)))
      t
      nil))

(defun set-flags (&rest keyword-args)
  (if (listp (first keyword-args))
      (let ((keywords 
	     (mapcar #'(lambda (x)
			 (eval x))
		     (first keyword-args))))
	(apply #'logior keywords))
      (apply #'logior keyword-args)))

(defun load-image (filename)
  "load in the supplied filename, must be a bmp file"
;  (format t "loading ~a~%" filename)
  (let ((file (namestring filename)))
    (if (and (stringp file) (probe-file file)) ; LJC: Make sure filename is a string and the filename exists.
	(sdl-cffi::SDL-Load-BMP-RW (sdl-cffi::sdl-RW-From-File file "rb") 1)
	(error "File ~A does not exist." file))))

