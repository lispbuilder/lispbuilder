
(in-package #:lispbuilder-sdl-cffi)

(cl:defconstant APP-MOUSE-FOCUS #x01)

(cl:defconstant APP-INPUT-FOCUS #x02)

(cl:defconstant APP-ACTIVE #x04)

(cffi:defcfun ("SDL_GetAppState" SDL-Get-App-State) :unsigned-char)
