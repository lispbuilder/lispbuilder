%insert("lisphead") 
%{
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Luke J Crook
;;;; See COPYING for license
;;;;
;;;; This .i file has been tested with SDL_mixer version 1.2.7

;;; To generate the SDL_mixer bindings using SWIG:
;;;  - Copy the "begin_code.h" header file from SDL-1.2.11 into the SDL_mixer include directory.
;;;    "begin_code.h" includes "#define SDLCALL __cdecl"

(in-package #:lispbuilder-sdl-mixer)

;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature)
;; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

;;;; Overrides to C header files follow:
;;;;

;;;;
;;;; end Overrides
%}

%module sdl_mixer
%{
// The following header files are converted by SWIG without errors.
#include "begin_code.h"	   // Complete for 1.2.7    // Copied from "SDL-1.2.11/incude/begin_code.h"
#include "SDL_mixer.h"	   // Complete for 1.2.7
%}

// "SDL_stdinc.h"
// Copied from SDL-1.2.11/include/SDL_stdinc.h"
typedef unsigned char	Uint8;
typedef signed char	Sint8;
typedef unsigned short	Uint16;
typedef signed short	Sint16;
typedef unsigned int	Uint32;
typedef signed int	Sint32;
// end "SDL_stdinc.h"

// "SDL_mixer.h"
// The following are defined in post-swig.i
%ignore SDL_MIXER_VERSION;
%ignore MIX_VERSION;
%ignore MIX_DEFAULT_FORMAT;
%ignore Mix_LoadWAV;
%ignore MIX_EFFECTSMAXSPEED;
%ignore Mix_PlayChannel;
%ignore Mix_FadeInChannel;
%ignore Mix_SetError;
%ignore Mix_GetError;
// end "SDL_mixer.h"

//Uncomment %feature to generate :exports
//%feature("export");

%include "begin_code.h" // This is copied from "SDL-1.2.11/incude/begin_code.h"
%include "SDL_mixer.h"
