
(in-package #:lispbuilder-openrm-cffi)

(cffi:defcfun ("wglCreateContext" #.(openrm-lispify "wglCreateContext" 'function)) :pointer
  (hdc :pointer))

(cffi:defcfun ("wglDeleteContext" #.(openrm-lispify "wglDeleteContext" 'function)) :int
  (hglrc :pointer))

