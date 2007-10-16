
(in-package #:lispbuilder-sdl-cffi)

(cffi:defcfun ("SDL_GetError" SDL-Get-Error) :string)

(cffi:defcfun ("SDL_ClearError" SDL-Clear-Error) :void)

(cffi:defcenum SDL-error-code
	:SDL-ENOMEM
	:SDL-EFREAD
	:SDL-EFWRITE
	:SDL-EFSEEK
	:SDL-UNSUPPORTED
	:SDL-LASTERROR)

(cffi:defcfun ("SDL_Error" SDL-Error) :void
  (code SDL-error-code))
