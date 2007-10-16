%insert("lisphead") 
%{
;;;; gl.h Mesa v6.4.2 CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

;;;; Note: (1) "gl.h" contains a LOT of typedef void (APIENTRYP blah blah blah) and
;;;;           none of these are yet defined.

(in-package #:lispbuilder-opengl)

(defctype GLenum :unsigned-int)
(defctype GLboolean :unsigned-char)
(defctype GLbitfield :unsigned-char)
(defctype GLvoid :void)
(defctype GLbyte :uint8)
(defctype GLshort :short)
(defctype GLint :int)
(defctype GLubyte :unsigned-char)
(defctype GLushort :unsigned-short)
(defctype GLuint :unsigned-int)
(defctype GLsizei :int)
(defctype GLfloat :float)
(defctype GLclampf :float)
(defctype GLdouble :double)
(defctype GLclampd :double)

%}

%module gl_1_1
%{
#include "gl_1_1.h"
%}

%ignore APIENTRYP;
%ignore GLAPIENTRYP;

%include "gl_1_1.h"
