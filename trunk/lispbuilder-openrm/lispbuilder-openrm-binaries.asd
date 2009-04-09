;;; -*- lisp -*-

(defpackage #:lispbuilder-openrm-binaries-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-openrm-binaries-system)

(defsystem lispbuilder-openrm-binaries
    :description "lispbuilder-openrm-binaries: The windows binaries for the OpenRM v1.6.0 library"
    :version "1.6.0"
    :author "R3vis Corporation ( www.openrm.org )"
    :maintainer "Application Builder <application-builder@lispniks.com>"
    :licence "LGPL"
    :components
    ((:module "bin"
	      :components
	      ((:static-file "librm.dll")
	       (:static-file "librmaux.dll")
	       (:static-file "librmi.dll")
	       (:static-file "librmv.dll")
	       #+win32 (:static-file "pthreadGC.dll")
	       #+win32 (:static-file "pthreadGCE.dll")
	       #+win32 (:static-file "pthreadVC.dll")
	       #+win32 (:static-file "pthreadVCE.dll")
	       #+win32 (:static-file "pthreadVSE.dll")
	       #+win32 (:static-file "msvcr70.dll")
	       #+win32 (:static-file "msvcrtd.dll")
	       ))
     (:module "documentation"
	      :components
	      ((:doc-file "bin_openrm_README")
	       (:doc-file "bin_openrm_VERSION")
	       (:doc-file "bin_openrm_LICENSE.html")
	       (:doc-file "bin_pthreads_COPYING")
	       (:doc-file "bin_pthreads_COPYING.LIB")
	       (:doc-file "bin_pthreads_README")))))
