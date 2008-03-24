
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
  

(cffi:defcfun ("PeekMessageA" PeekMessage) :int
  (lpMsg :pointer)
  (hWnd :pointer)
  (wMsgFilterMin :unsigned-int)
  (wMsgFilterMax :unsigned-int)
  (wRemoveMsg :unsigned-int))

(cl:defconstant PM_NOREMOVE #x0000)
(cl:defconstant PM_REMOVE   #x0001)
(cl:defconstant PM_NOYEiLD  #x0002)
