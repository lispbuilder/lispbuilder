
(in-package #:lispbuilder-windows)

(cffi:defcfun ("ValidateRect" Validate-Rect) :int
  (hwnd :pointer)
  (rect :pointer))

(cffi:defcfun ("GetWindowRect" Get-Window-Rect) :int
  (hwnd :pointer)
  (lpRect :pointer))

(cffi:defcstruct CREATESTRUCT
  (lpCreateParams :pointer)
  (hInstance :pointer)
  (hMenu :pointer)
  (hwndParent :pointer)
  (cy :int)
  (cx :int)
  (y :int)
  (x :int)
  (style :long)
  (lpszClass :string)
  (lpszClassName :string)
  (lpszClass :pointer))
  
