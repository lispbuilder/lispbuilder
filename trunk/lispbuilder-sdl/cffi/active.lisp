
(in-package #:lispbuilder-sdl-cffi)

(cl:defconstant SDL-APP-MOUSE-FOCUS #x01)

(cl:defconstant SDL-APP-INPUT-FOCUS #x02)

(cl:defconstant SDL-APP-ACTIVE #x04)

(cffi:defcfun ("SDL_GetAppState" SDL-Get-App-State) :unsigned-char)
