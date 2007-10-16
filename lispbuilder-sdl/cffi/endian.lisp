
(in-package #:lispbuilder-sdl-cffi)

;;; First, set the byte-order. This is probably not needed.
(defconstant SDL-LIL-ENDIAN 1234)
(defconstant SDL-BIG-ENDIAN 4321)

;;; Set the byte order for the current CPU
#-(or little-endian PC386 X86 I386) (defconstant SDL-BYTE-ORDER SDL-BIG-ENDIAN)
#+(or little-endian PC386 X86 I386) (defconstant SDL-BYTE-ORDER SDL-LIL-ENDIAN)

