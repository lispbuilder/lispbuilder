;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)


(defmacro check-bounds (min below &rest vars)
  (let (result)
    (loop for var in vars do
	 (push `(setf ,var (clamp ,var ,min ,below)) result))
    (push 'progn result)
    result))

;;; w
(defmacro with-init (init-flags &body body)
  "Attempts to initialize the SDL subsystems using SDL_Init.
   Automatically shuts down the SDL subsystems using SDL_Quit upon normal application termination or
   if any fatal error occurs within &body.
   init-flags can be any combination of SDL_INIT_TIMER, SDL_INIT_AUDIO, SDL_INIT_VIDEO, SDL_INIT_CDROM,
   SDL_INIT_JOYSTICK, SDL_INIT_NOPARACHUTE, SDL_INIT_EVENTTHREAD or SDL_INIT_EVERYTHING."
  `(block nil
    (unwind-protect
	 (when (init-sdl :flags (list ,@init-flags))
	   ,@body)
      (SDL_Quit))))

(defun init-sdl (&key (flags SDL_INIT_VIDEO))
  (if (equal 0 (SDL_Init (set-flags flags)))
      t
      nil))

(defun key= (key1 key2)
  (eq key1 key2))

(defun modifier= (mod key)
  "Returns t if the keypress modifier 'mod' is equal to the specified 'key'.
   (cffi:foreign-enum-value 'SDLMod key)."
  (equal mod (cffi:foreign-enum-value 'SDLMod key)))

;; cl-sdl "util.lisp"
(declaim (inline clamp))
(defun clamp (v l u)
  (min (max v l) u))

(defun clamp-to-sbyte (v)
  (min (max v -127) 127))

(defun clamp-to-ubyte (v)
  (min (max v 0) 255))

(defun clamp-to-sshort (v)
  (min (max v -32767) 32767))

(defun clamp-to-ushort (v)
  (min (max v 0) 65535))

;; cl-sdl "util.lisp"
(defun delta-clamp (v d l u)
  (let ((sum (+ v d)))
    (cond ((< sum l)
           (- d (- sum l)))
          ((> sum u)
           (- d (- sum u)))
          (t d))))

(defun is-valid-ptr (pointer)
  "IS-VALID-PTR <CFFI pointer>
  Will return T if 'pointer' is a valid <CFFI pointer> and is non-null."
  (and (cffi:pointerp pointer) (not (cffi:null-pointer-p pointer))))

(defun set-flags (&rest keyword-args)
  (if (listp (first keyword-args))
      (let ((keywords 
	     (mapcar #'(lambda (x)
			 (eval x))
		     (first keyword-args))))
	(apply #'logior keywords))
      (apply #'logior keyword-args)))
