
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

;;; Is this even cross platform between Windows, *nix, OSX?
;; extern DECLSPEC char * SDLCALL SDL_getenv(const char *name);
(if (cffi:foreign-symbol-pointer "getenv")
    (defcfun ("getenv" SDL-get-env) :pointer
      (variable :string))
    (defcfun ("SDL_getenv" SDL-get-env) :pointer
	    (name :string)))

;;; Is this even cross platform between Windows, *nix, OSX?
;; extern DECLSPEC int SDLCALL SDL_putenv(const char *variable);
(if (cffi:foreign-symbol-pointer "putenv")
    (defcfun ("putenv" SDL-put-env) :int
      (variable :string))
    (defcfun ("SDL_putenv" SDL-put-env) :int
      (variable :string)))
