;;; -*- lisp -*-

(in-package #:lispbuilder-openrm-cffi)


#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
		  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
			   cffi:*foreign-library-directories*
			   :test #'equal))

#+win32(cffi:define-foreign-library msvcr70
  (:windows "msvcr70.dll"))
#+win32(cffi:define-foreign-library msvcrtd
  (:windows "msvcrtd.dll"))
#+win32(cffi:define-foreign-library pthreadGC
  (:windows "pthreadGC.dll"))
#+win32(cffi:define-foreign-library pthreadVC
  (:windows "pthreadVC.dll"))
#+win32(cffi:define-foreign-library pthreadVCE
  (:windows "pthreadVCE.dll"))
#+win32(cffi:define-foreign-library pthreadVSE
  (:windows "pthreadVSE.dll"))

(cffi:define-foreign-library librm
  (:darwin (:framework "librm"))
  (:windows "librm.dll")
  (:unix (:or "librm" "librm.so" "librm.so.1.6")))

(cffi:define-foreign-library librmaux
  (:darwin (:framework "librmaux"))
  (:windows "librmaux.dll")
  (:unix (:or "librmaux" "librmaux.so" "librmaux.so.1.6")))

#+win32(cffi:define-foreign-library librmi
  (:darwin (:framework "librmi"))
  (:windows "librmi.dll")
  (:unix (:or "librmi" "librmi.so" "librmi.so.1.6")))

(cffi:define-foreign-library librmv
  (:darwin (:framework "librmv"))
  (:windows "librmv.dll")
  (:unix (:or "librmv" "librmv.so" "librmv.so.1.6")))

;;; From cl-opengl
;;; http://common-lisp.net/project/cl-opengl/darcs/cl-opengl/glu/library.lisp
(cffi:define-foreign-library GL
  (:darwin (:framework "OpenGL"))
  (:windows "opengl32.dll" :calling-convention :stdcall)
  (:unix (:or "libGL.so" "libGL.so.2" "libGL.so.1")))

;;; From cl-opengl
;;; http://common-lisp.net/project/cl-opengl/darcs/cl-opengl/glu/library.lisp
;;; On darwin GLU is part of the OpenGL framework and thus
;;; is loaded already by cl-opengl, on which cl-glu depends.
(define-foreign-library GLU
  (:windows "glu32.dll") ; XXX?
  ((:and :unix (:not :darwin)) (:or "libGLU.so.1" "libGLU.so"))
  ((:not :darwin) (:default "libGLU")))

#+win32(cffi:use-foreign-library msvcr70)
#+win32(cffi:use-foreign-library msvcrtd)
#+win32(cffi:use-foreign-library pthreadGC)
#+win32(cffi:use-foreign-library pthreadVC)
#+win32(cffi:use-foreign-library pthreadVCE)
#+win32(cffi:use-foreign-library pthreadVSE)

(cffi:use-foreign-library librm)
(cffi:use-foreign-library librmaux)
#+win32(cffi:use-foreign-library librmi)
(cffi:use-foreign-library librmv)

(cffi:use-foreign-library GL)
#+win32(cffi:use-foreign-library GLU)
