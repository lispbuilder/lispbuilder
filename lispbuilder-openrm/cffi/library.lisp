;;; -*- lisp -*-

(in-package #:lispbuilder-openrm-cffi)


;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (pushnew (merge-pathnames "../bin/" (directory-namestring (or *load-truename* *default-pathname-defaults*)))
;;           cffi:*foreign-library-directories*
;;           :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew rm-bin:*dll-path*
	   cffi:*foreign-library-directories*
	   :test #'equal))

(cffi:define-foreign-library msvcr70
  (:windows "msvcr70.dll"))
(cffi:define-foreign-library msvcrtd
  (:windows "msvcrtd.dll"))
(cffi:define-foreign-library pthreadGC
  (:windows "pthreadGC.dll"))
(cffi:define-foreign-library pthreadVC
  (:windows "pthreadVC.dll"))
(cffi:define-foreign-library pthreadVCE
  (:windows "pthreadVCE.dll"))
(cffi:define-foreign-library pthreadVSE
  (:windows "pthreadVSE.dll"))

(cffi:define-foreign-library librm
  (:darwin (:framework "librm"))
  (:windows "librm.dll")
  (:unix (:or "librm" "librm.so" "librm.so.1.6")))

(cffi:define-foreign-library librmaux
  (:darwin (:framework "librmaux"))
  (:windows "librmaux.dll")
  (:unix (:or "librmaux" "librmaux.so" "librmaux.so.1.6")))

(cffi:define-foreign-library librmi
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

(defun load-library ()
  (when (handler-case (cffi:use-foreign-library msvcr70)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library msvcrtd)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library pthreadGC)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library pthreadVC)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library pthreadVCE)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library pthreadVSE)
          (load-foreign-library-error () nil)))

  (when (handler-case (cffi:use-foreign-library librm)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library librmaux)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library librmi)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library librmv)
          (load-foreign-library-error () nil)))

  (when (handler-case (cffi:use-foreign-library GL)
          (load-foreign-library-error () nil)))
  (when (handler-case (cffi:use-foreign-library GLU)
          (load-foreign-library-error () nil))))

(eval-when (:load-toplevel :execute)
  (load-library))
