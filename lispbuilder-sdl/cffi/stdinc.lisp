
(in-package #:lispbuilder-sdl-cffi)


;;; Probably do not need this.
(defcenum SDL-bool
	(:SDL-FALSE 0)
	(:SDL-TRUE 1))

;;; Probably do not need this.
(defcstruct Uint64
	(hi :unsigned-int)
	(lo :unsigned-int))

;;; Probably do not need this.
(defcenum SDL-DUMMY-ENUM
	:DUMMY-ENUM-VALUE)

;; extern DECLSPEC char * SDLCALL SDL_getenv(const char *name);
(defun sdl-get-env (string)
  (when (cffi:foreign-symbol-pointer "getenv")
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "getenv") () :string string :pointer))
  (when (cffi:foreign-symbol-pointer "SDL_getenv" :library 'sdl)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_getenv") () :string string :pointer)))

;; extern DECLSPEC int SDLCALL SDL_putenv(const char *variable);
(defun sdl-put-env (string)
  (when (cffi:foreign-symbol-pointer "putenv")
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "putenv") () :string string :int))
  (when (cffi:foreign-symbol-pointer "SDL_putenv" :library 'sdl)
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "SDL_putenv") () :string string :int)))
