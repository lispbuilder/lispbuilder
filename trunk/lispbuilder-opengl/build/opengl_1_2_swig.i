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

%}

%module gl_1_2
%{
#include "gl_1_1.h"
#include "gl_1_2.h"
%}

%ignore APIENTRYP;
%ignore GLAPIENTRYP;

%import "gl_1_1.h"
%include "gl_1_2.h"
