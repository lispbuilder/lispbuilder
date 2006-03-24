;;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :lispbuilder-opengl
  (:nicknames :gl)
  (:use :common-lisp :cffi)
  (:documentation "The main package of `lispbuilder-opengl'."))

(in-package :lispbuilder-opengl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+win32 (load-foreign-library "OPENGL32.dll")
  ;;  #+win32 (load-foreign-library "GLU32.dll")
  #-win32 (load-foreign-library "/usr/X11R6/lib/libGL.so")
  #-win32 (load-foreign-library "/usr/X11R6/lib/libGLU.so"))
