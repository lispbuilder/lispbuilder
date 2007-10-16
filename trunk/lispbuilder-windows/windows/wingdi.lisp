
(in-package #:lispbuilder-windows)


(cl:defconstant PFD_DRAW_TO_WINDOW 4)
(cl:defconstant PFD_SUPPORT_OPENGL 32)
(cl:defconstant PFD_DOUBLEBUFFER 1)
(cl:defconstant PFD_TYPE_RGBA 0)
(cl:defconstant PFD_MAIN_PLANE 0)

(cffi:defcfun ("SetPixelFormat" Set-Pixel-Format) :int
  (hdc :pointer)
  (iPixelFormat :int)
  (ppfd :pointer))

(cffi:defcfun ("ChoosePixelFormat" Choose-Pixel-Format) :int
  (hdc :pointer)
  (ppfd :pointer))

(cffi:defcstruct PIXELFORMATDESCRIPTOR
  (nSize :short) ; WORD  nSize; 
  (nVersion :short) ;   WORD  nVersion; 
  (dwFlags :unsigned-long) ;   DWORD dwFlags; 
  (iPixelType :char) ;   BYTE  iPixelType; 
  (cColorBits :char) ;   BYTE  cColorBits; 
  (cRedBits :char) ;   BYTE  cRedBits; 
  (cRedShift :char) ;   BYTE  cRedShift; 
  (cGreenBits :char) ;   BYTE  cGreenBits; 
  (cGreenShift :char) ;  BYTE  cGreenShift; 
  (cBlueBits :char) ;   BYTE  cBlueBits; 
  (cBlueShift :char) ;  BYTE  cBlueShift; 
  (cAlphaBits :char) ;  BYTE  cAlphaBits; 
  (cAlphaShift :char) ;   BYTE  cAlphaShift; 
  (cAccumBits :char) ;   BYTE  cAccumBits; 
  (cAccumRedBits :char) ;   BYTE  cAccumRedBits; 
  (cAccumGreenBits :char) ;   BYTE  cAccumGreenBits; 
  (cAccumBlueBits :char) ;   BYTE  cAccumBlueBits; 
  (cAccumAlphaBits :char) ;   BYTE  cAccumAlphaBits; 
  (cDepthBits :char) ;   BYTE  cDepthBits; 
  (cStencilBits :char) ;   BYTE  cStencilBits; 
  (cAuxBuffers :char) ;   BYTE  cAuxBuffers; 
  (iLayerType :char) ;  BYTE  iLayerType; 
  (bReserved :char) ;   BYTE  bReserved; 
  (dwLayerMask :unsigned-long) ;   DWORD dwLayerMask; 
  (dwVisibleMask :unsigned-long) ;   DWORD dwVisibleMask; 
  (dwDamageMask :unsigned-long)) ;   DWORD dwDamageMask; 













