
(in-package #:lispbuilder-openrm-cffi)

(cffi:defcfun ("wglCreateContext" #.(openrm-lispify "wglCreateContext" 'function)) :pointer
  (hdc :pointer))

(cffi:defcfun ("wglDeleteContext" #.(openrm-lispify "wglDeleteContext" 'function)) :int
  (hglrc :pointer))

(cffi:defcfun ("wglGetCurrentDC" #.(openrm-lispify "wglGetCurrentDC" 'function)) :pointer)

(cffi:defcfun ("wglGetCurrentContext" #.(openrm-lispify "wglGetCurrentContext" 'function)) :pointer)

(cffi:defcfun ("wglMakeCurrent" #.(openrm-lispify "wglMakeCurrent" 'function)) :boolean
  (hdc :pointer)
  (hglrc :pointer))

(cffi:defcfun ("wglSwapBuffers" #.(openrm-lispify "wglSwapBuffers" 'function)) :boolean
  (hdc :pointer))

;;BOOL wglMakeCurrent(
;;    HDC hdc,
;;    HGLRC hglrc
;;);