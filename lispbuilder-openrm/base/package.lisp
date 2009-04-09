;;;; lispbuilder-openrm-base

(in-package #:cl-user)

(defpackage #:lispbuilder-openrm-base
  (:use #:cl #:cffi)
  (:nicknames #:rm-base)
  (:documentation "The main package of `lispbuilder-openrm-base'.")
  (:import-from #:lispbuilder-openrm-cffi
		lispbuilder-openrm-cffi::r
		lispbuilder-openrm-cffi::g
		lispbuilder-openrm-cffi::b
		lispbuilder-openrm-cffi::a
		lispbuilder-openrm-cffi::rm-color-4d
		lispbuilder-openrm-cffi::rm-color-3d
		lispbuilder-openrm-cffi::rm-vertex-3d
		lispbuilder-openrm-cffi::rm-vertex-2d
		lispbuilder-openrm-cffi::x
		lispbuilder-openrm-cffi::y
		lispbuilder-openrm-cffi::z
		lispbuilder-openrm-cffi::w)
  (:export

   ;; utils.lisp
   #:with-c3d
   #:with-c4d
   #:with-rm-light
   #:with-v2d
   #:with-v3d
   #:with-v4d
   #:x #:y #:z #:w
   #:r #:g #:b #:a))
   