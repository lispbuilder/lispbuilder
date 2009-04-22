;;;; gl.h Mesa v6.4.2 CFFI lisp wrapper
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

(in-package #:rm-cffi)

(defconstant GL_VERSION_1_1 1)

(defconstant GL_VERSION_1_2 1)

(defconstant GL_VERSION_1_3 1)

(defconstant GL_VENDOR #x1F00)

(defconstant GL_RENDERER #x1F01)

(defconstant GL_VERSION #x1F02)

(defconstant GL_EXTENSIONS #x1F03)

(defcfun ("glGetString" glGetString) :string
  (name :unsigned-int))

(defconstant GLU_VERSION 100800)
(defconstant GLU_EXTENSIONS 100801)

(defcfun ("gluGetString" glugetstring) :string
  (name :unsigned-int))
