

(in-package :lispbuilder-windows) 

(cffi:defcfun ("GetDC" get-dc) :pointer
  (hwnd :pointer))

(cffi:defcfun ("DestroyWindow" Destroy-Window) :int
  (hwnd :pointer))
