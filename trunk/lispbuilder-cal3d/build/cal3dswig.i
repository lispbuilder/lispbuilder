%insert("lisphead") 
%{
;;;; Cal3D v0.10.0 CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license

(in-package #:lispbuilder-cal3d)

;;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature
; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

(defcenum Boolean
	(:False 0)
	(:True 1))

(defcenum CalAnimationType
	(:ANIMATION_TYPE_NONE 0)
	:ANIMATION_TYPE_CYCLE
	:ANIMATION_TYPE_POSE
	:ANIMATION_TYPE_ACTION)

(defcenum CalAnimationState
	(:ANIMATION_STATE_NONE 0)
	:ANIMATION_STATE_SYNC
	:ANIMATION_STATE_ASYNC
	:ANIMATION_STATE_IN
	:ANIMATION_STATE_STEADY
	:ANIMATION_STATE_OUT)

(defcenum CalErrorCode
	(:ERROR_CODE_OK 0)
	:ERROR_CODE_INTERNAL
	:ERROR_CODE_INVALID_HANDLE
	:ERROR_CODE_MEMORY_ALLOCATION_FAILED
	:ERROR_CODE_FILE_NOT_FOUND
	:ERROR_CODE_INVALID_FILE_FORMAT
	:ERROR_CODE_FILE_PARSER_FAILED
	:ERROR_CODE_INDEX_BUILD_FAILED
	:ERROR_CODE_NO_PARSER_DOCUMENT
	:ERROR_CODE_INVALID_ANIMATION_DURATION
	:ERROR_CODE_BONE_NOT_FOUND
	:ERROR_CODE_INVALID_ATTRIBUTE_VALUE
	:ERROR_CODE_INVALID_KEYFRAME_COUNT
	:ERROR_CODE_INVALID_ANIMATION_TYPE
	:ERROR_CODE_FILE_CREATION_FAILED
	:ERROR_CODE_FILE_WRITING_FAILED
	:ERROR_CODE_INCOMPATIBLE_FILE_VERSION
	:ERROR_CODE_NO_MESH_IN_MODEL
	:ERROR_CODE_MAX_ERROR_CODE)

(defcfun ("CalCoreAnimation_New" CalCoreAnimation_New) :pointer
  (a-void :pointer))

%}

%module cal3d
%{
//#include "animation.h"
#include "cal3d_wrapper.h"
%}

%ignore Boolean;
%ignore CalAnimationType;
%ignore CalAnimationState;
%ignore CalErrorCode;
%ignore CalCoreAnimation_New;

typedef int Boolean;
typedef int CalAnimationType;
typedef int CalAnimationState;
typedef int CalErrorCode;

//%include "animation.h"
%include "cal3d_wrapper.h"
