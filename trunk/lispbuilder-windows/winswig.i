%include "windows.i"

%insert("lisphead") 
%{
(in-package :win)

;;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))
%}

%module windows
%{

%include "windows.h"

%}

%include "windows.h"
