
(in-package #:lispbuilder-sdl-cffi) 

(cffi:defcstruct SDL-Cursor
	(area :pointer)
	(hot-x :short)
	(hot-y :short)
	(data :pointer)
	(mask :pointer)
	(save :pointer)
	(wm-cursor :pointer))

(cffi:defcfun ("SDL_GetMouseState" SDL-Get-Mouse-State) :unsigned-char
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_GetRelativeMouseState" SDL-Get-Relative-Mouse-State) :unsigned-char
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_CreateCursor" SDL-Create-Cursor) :pointer
  (data :pointer)
  (mask :pointer)
  (w :int)
  (h :int)
  (hot-x :int)
  (hot-y :int))

(cffi:defcfun ("SDL_SetCursor" SDL-Set-Cursor) :void
  (cursor :pointer))

(cffi:defcfun ("SDL_GetCursor" SDL-Get-Cursor) :pointer)

(cffi:defcfun ("SDL_FreeCursor" SDL-Free-Cursor) :void
  (cursor :pointer))

(cffi:defcfun ("SDL_ShowCursor" SDL-Show-Cursor) :int
  (toggle :int))

(cl:defconstant SDL-BUTTON-LEFT 1)
(cl:defconstant SDL-BUTTON-MIDDLE 2)
(cl:defconstant SDL-BUTTON-RIGHT 3)
(cl:defconstant SDL-BUTTON-WHEEL-UP 4)
(cl:defconstant SDL-BUTTON-WHEEL-DOWN 5)
(cl:defconstant SDL-BUTTON-X1 6)
(cl:defconstant SDL-BUTTON-X2 7)
;; (cl:defconstant SDL-BUTTON-WHEEL-LEFT 6)
;; (cl:defconstant SDL-BUTTON-WHEEL-RIGHT 7)

(cl:defconstant SDL-RELEASED 0)
(cl:defconstant SDL-PRESSED 1)

(cffi:defcfun ("SDL_WarpMouse" sdl-Warp-Mouse) :void
  (x :unsigned-short)
  (y :unsigned-short))

(defun SDL-BUTTON (X)
  (1<< (- X 1)))

(defun SDL-BUTTON-LMASK ()
  (SDL-BUTTON SDL-BUTTON-LEFT))

(defun SDL-BUTTON-MMASK ()
  (SDL-BUTTON SDL-BUTTON-MIDDLE))

(defun SDL-BUTTON-RMASK ()
  (SDL-BUTTON SDL-BUTTON-RIGHT))

(defun SDL-BUTTON-X1MASK ()
  (SDL-BUTTON SDL-BUTTON-X1))

(defun SDL-BUTTON-X2MASK ()
  (SDL-BUTTON SDL-BUTTON-X2))
