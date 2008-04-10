%insert("lisphead") 
%{
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2008 Luke J Crook <luke@balooga.com>
;;;; See COPYING for license
;;;;
;;;; This .i file successfully generates CFFI bindings for SDL_mixer version 1.2.8
;;;;
;;;; SWIG command line:
;;;; swig -cffi -I<path_to_SDL_mixer_includes> -I<path_to_SDL_includes> -Ilib -Ilib\cffi <location_of_openrmswig.i>

(in-package #:lispbuilder-sdl-mixer-cffi)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'sdlmixer-lispify)
    (defun sdlmixer-lispify (name flag &optional (package *package*))
      (labels ((find-sub (src lst)
		 (when (>= (length lst)
			   (length src))
		   (if (and (equal src (subseq lst 0 (length src)))
			    (not (equal (nth (length src)
					     lst) #\_))
			    (not (null (nth (length src)
					    lst))))
		       t
		       nil)))
	       (replace-sub (new old lis)
		 (append new (nthcdr (length old) lis)))
	       (remove-sub (old lis)
		 (nthcdr (length old) lis))
	       (next-char (char)
		 (if char
		     (cond
		       ((upper-case-p char)
			'upper)
		       ((lower-case-p char)
			'lower)
		       (t nil))
		     nil))
	       (helper (lst last prev-last rest &aux (c (car lst)))
		 (declare (ignore prev-last))
		 (cond
		   ((null lst)
		    rest)
		   ((upper-case-p c)
		    (let ((old '((#\M #\i #\x #\_) (#\M #\I #\X #\_))))
		      (dolist (str old)
			(when (find-sub str lst)
      			(setf lst (remove-sub str lst)
			      c (first lst)))))
		    (helper (cdr lst) 'upper last
			    (cond
			      ((or (equal last 'lower)
				   ;; (equal last 'digit)
				   )
			       (list* c #\- rest))
			      ((and (equal last 'upper)
				    (equal (next-char (cadr lst)) 'lower))
			       (list* c #\- rest))
			      (t (cons c rest)))))
		   ((lower-case-p c)
		    (helper (cdr lst) 'lower last (cons (char-upcase c) rest)))
		   ((digit-char-p c)
		    (helper (cdr lst) 'digit last
			    (case last
			      ((upper lower) (list* c #\- rest))
			      (t (cons c rest)))))
		   ((char-equal c #\_)
		    (helper (cdr lst) '_ last (cons #\- rest)))
		   (t
		    (error "Invalid character: ~A" c)))))
	(let ((fix (case flag
		     ((constant variable) "+")
		     (enumvalue "")
		     (t ""))))
	  (intern
	   (concatenate
	    'string
	    fix
	    (nreverse (helper (concatenate 'list name) nil nil nil))
	    fix)
	   package))))))

;;;; Lispifies the following 'C' keywords:
;;;; scancode 		=    SCANCODE
;;;; SDL_ALL_HOTKEYS 	=    *SDL-ALL-HOTKEYS*
;;;; SDLKey 		=    SDL-KEY
;;;; RMenum		=    RM-ENUM
;;;; SDL_GetKeyRepeat	=    SDL-GET-KEY-REPEAT
;;;; SDL_RWFromFP	=    SDL-RW-FROM-FP
;;;; SDL_HasSSE 	=    SDL-HAS-SSE
;;;; SDL_HasSSE2 	=    SDL-HAS-SSE-2
;;;; RMcolor4D		=    RM-COLOR-4D
;;;; SDL_Has3DNow 	=    SDL-HAS-3D-NOW
;;;; SDL_WriteBE32	=    SDL-WRITE-BE-32
;;;; SDLK_SLASH		=    :KEY-SLASH
;;;; SDLK_F1		=    :KEY-F-1
;;;; KMOD_LSHIFT	=    :KEY-MOD-LSHIFT

;;;; These defctypes are used by the CFFI translation functions
;;;; see the typemap definition below.
(cffi:defctype mix-chunk-fp (:wrapper :pointer 
				      :to-c to-mix-chunk))
(cffi:defctype mix-music-fp (:wrapper :pointer 
				      :to-c to-mix-music))



;;;; Overrides to C header files follow:
;;;;

;;;;
;;;; end Overrides
%}

%module sdl_mixer
%feature("intern_function","sdlmixer-lispify");
%typemap(cin) Mix_Chunk* "mix-chunk-fp";
%typemap(cin) Mix_Music* "mix-music-fp";


%{
// The following header files are converted by SWIG without errors.
#include "begin_code.h"	   // Complete for 1.2.8
#include "SDL_mixer.h"	   // Complete for 1.2.8
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
// See below.
%ignore SDL_MIXER_VERSION;
%ignore MIX_VERSION;
%ignore MIX_DEFAULT_FORMAT;
%ignore MIX_EFFECTSMAXSPEED;
%ignore Mix_LoadWAV;
%ignore Mix_PlayChannel;
%ignore Mix_FadeInChannel;
%ignore Mix_SetError;
%ignore Mix_GetError;

// The following functions are defined below.
// The finalize method for TRIVIAL-GARBAGE passes the foreign pointer to 
// the free functions.
%ignore Mix_FreeChunk;
%ignore Mix_FreeMusic;

// end "SDL_mixer.h"

//Uncomment %feature to generate :exports
//%feature("export");

%include "begin_code.h"
%include "SDL_mixer.h"


%insert("lisphead") %{
;;;; "SDL_mixer.h"

(cffi:defcfun ("Mix_FreeChunk" FREE-CHUNK) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" FREE-MUSIC) :void
  (music :pointer))

(defun SDL-MIXER-VERSION (x)
  (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) x sdl-cffi::sdl-version)
    (setf sdl-cffi::major +SDL-MIXER-MAJOR-VERSION+
          sdl-cffi::minor +SDL-MIXER-MINOR-VERSION+
          sdl-cffi::patch +SDL-MIXER-PATCHLEVEL+)))

(defun VERSION (x)
  (SDL-MIXER-VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16LSB) ;; Little Endian

(defun Load-WAV (file)
  (Load-WAV-RW (sdl-cffi::SDL-RW-FROM-FILE file "rb") 1))

(defun Play-Channel (channel chunk loops)
  (Play-Channel-Timed channel chunk loops -1))

(defun Fade-In-Channel (channel chunk loops ms)
  (Fade-In-Channel-Timed channel chunk loops ms -1))

(defun Get-Error ()
  (sdl-cffi::SDL-Get-Error))

%}