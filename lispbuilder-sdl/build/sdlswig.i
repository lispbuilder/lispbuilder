%insert("lisphead") 
%{
;;;; SDL CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Justin Heyes-Jones, Luke J Crook
;;;; See COPYING for license
;;;;
;;;; This .i file has been tested with SDL version 1.2.11

;; include/*.h changes from SDL-1.2.9 to SDL-1.2.11
;;
;;	SDL-1.2.9		SDL-1.2.11	 
;;	==========		=========	 
;;	SDL.h			SDL.h		 
;;	SDL_active.h		SDL_active.h	 
;;	SDL_audio.h		SDL_audio.h	     
;;	SDL_byteorder.h		SDL_byteorder.h	 
;;	SDL_cdrom.h		SDL_cdrom.h	     
;;	SDL_copying.h		SDL_copying.h	 
;;	SDL_cpuinfo.h		SDL_cpuinfo.h	 
;;	SDL_endian.h		SDL_endian.h	 
;;	SDL_error.h		SDL_error.h	 
;;	SDL_events.h		SDL_events.h	 
;;	SDL_getenv.h		SDL_getenv.h	 
;;	SDL_joystick.h		SDL_joystick.h	 
;;	SDL_keyboard.h		SDL_keyboard.h	 
;;	SDL_keysym.h		SDL_keysym.h	 
;;	SDL_loadso.h		SDL_loadso.h	 
;;	SDL_main.h		SDL_main.h	    
;;	SDL_mouse.h		SDL_mouse.h	    
;;	SDL_mutex.h		SDL_mutex.h	    
;;	SDL_name.h		SDL_name.h	    
;;	SDL_opengl.h		SDL_opengl.h	 
;;				SDL_platform.h	 
;;	SDL_quit.h		SDL_quit.h	 
;;	SDL_rwops.h		SDL_rwops.h	 
;;				SDL_stdinc.h	 
;;	SDL_syswm.h		SDL_syswm.h	 
;;	SDL_thread.h		SDL_thread.h	 
;;	SDL_timer.h		SDL_timer.h	 
;;	SDL_types.h		SDL_types.h	 
;;	SDL_version.h		SDL_version.h	 
;;	SDL_video.h		SDL_video.h	 
;;	begin_code.h		begin_code.h	 
;;	close_code.h		close_code.h	 
;;
;; The following #includes are not processed by sdlswig.i
;;  - "SDL_syswm.h" 	   // Too complicated. Partially defined in sdlswig.i	// Complete for 1.2.11
;;  - "SDL_platform.h" 	   // Nothing to be done.	   	    		// Complete for 1.2.11
;;  - "SDL_stdinc.h" 	   // Too complicated. Partially defined in sdlswig.i   // Complete for 1.2.11
;;  - "SDL_getenv.h"	   // Depreciated. Now in "SDL_stdinc.h"    		// Complete for 1.2.11
;;  - "SDL_types.h"  	   // Depreciated. Now in "SDL_stdinc.h"		// Complete for 1.2.11
;;  - "SDL_byteorder.h"	   // Depreciated. Now in "SDL_endian.h"		// Complete for 1.2.11
;;  - "SDL_endian.h"  	   // Too complicated. Partially defined in sdlswig.i	// Complete for 1.2.11
;;  - "SDL_thread.h" 	   // Use native Lisp threads instead. 			// Complete for 1.2.11
;;  - "SDL_mutex.h" 	   // Use native Lisp threads instead. 			// Complete for 1.2.11
;;  - "SDL_timer.h" 	   // Necessary functions are defined in sdlswig.i	// Complete for 1.2.11
;;  - "SDL_opengl.h" 	   // Use CL-OPENGL instead.  	      	 		// Complete for 1.2.11

(in-package #:lispbuilder-sdl)

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
;;; "SDL_endian.h"
;;; First, set the byte-order. This is probably not needed.
(defconstant SDL_LIL_ENDIAN 1234)
(defconstant SDL_BIG_ENDIAN 4321)

;;; Set the byte order for the current CPU
#-(or little-endian PC386 X86 I386) (defconstant SDL_BYTEORDER SDL_BIG_ENDIAN)
#+(or little-endian PC386 X86 I386) (defconstant SDL_BYTEORDER SDL_LIL_ENDIAN)
;;; End "SDL_endian.h"

;;; "SDL_video.h"
;;; Here we define SDL_VideoInfo as it uses nasty bitfields which SWIG does not yet generate automatic wrappers for.
(defbitfield hardware-flags
  (:hw_available #x0000)
  (:wm_available #x0001)
  (:blit_hw #x0200)
  (:blit_hw_CC #x0400)
  (:blit_hw_A #x0800)
  (:blit_sw #x1000)
  (:blit_sw_CC #x2000)
  (:blit_sw_A #x4000)
  (:blit_fill #x8000))

(defcstruct SDL_VideoInfo
  (flags hardware-flags)
  (video_mem :unsigned-int)
  (vfmt :pointer)
  (current_w :int)	;; New for SDL-1.2.11
  (current_h :int))	;; New for SDL-1.2.11

;;; end "SDL_video.h"

;;;; "SDL_stdinc.h"
;;; Probably do not need this.
(defcenum SDL_bool
	(:SDL_FALSE 0)
	(:SDL_TRUE 1))

;;; Probably do not need this.
(defcstruct Uint64
	(hi :unsigned-int)
	(lo :unsigned-int))

;;; Probably do not need this.
(defcenum SDL_DUMMY_ENUM
	:DUMMY_ENUM_VALUE)

;;; Is this even cross platform between Windows, *nix, OSX?
;; extern DECLSPEC char * SDLCALL SDL_getenv(const char *name);
(defcfun ("SDL_getenv" SDL_getenv) :pointer
  (name :string))

;;; Is this even cross platform between Windows, *nix, OSX?
;; extern DECLSPEC int SDLCALL SDL_putenv(const char *variable);
(defcfun ("SDL_putenv" SDL_putenv) :int
  (variable :string))
;;;; end "SDL_stdinc.h"

;;; "SDL_timer.h"
;;; These are really the only functions we require from "SDL_timer.h"
;;/* Get the number of milliseconds since the SDL library initialization.
;; * Note that this value wraps if the program runs for more than ~49 days.
;; */ 
;;extern DECLSPEC Uint32 SDLCALL SDL_GetTicks(void);
(defcfun ("SDL_GetTicks" SDL_GetTicks) :unsigned-int)

;;/* Wait a specified number of milliseconds before returning */
;;extern DECLSPEC void SDLCALL SDL_Delay(Uint32 ms);
(defcfun ("SDL_Delay" SDL_Delay) :void
  (ms :unsigned-int))
;;;; end "SDL_timer.h"


;;;;
;;;; end Overrides
%}

%module sdl
%{
// The following header files are converted by SWIG without errors.
#include "begin_code.h"	   // Complete for 1.2.11
#include "SDL_main.h" 	   // Complete for 1.2.11
#include "SDL_error.h" 	   // Complete for 1.2.11
#include "SDL_rwops.h" 	   // Complete for 1.2.11
#include "SDL_audio.h" 	   // Complete for 1.2.11	
#include "SDL_cdrom.h" 	   // Complete for 1.2.11	
#include "SDL_joystick.h"  // Complete for 1.2.11
#include "SDL_active.h"    // Complete for 1.2.11	
#include "SDL_keysym.h"    // Complete for 1.2.11	
#include "SDL_keyboard.h"  // Complete for 1.2.11	
#include "SDL_mouse.h" 	   // Complete for 1.2.11	
#include "SDL_joystick.h"  // Complete for 1.2.11	
#include "SDL_quit.h" 	   // Complete for 1.2.11	
#include "SDL_events.h"    // Complete for 1.2.11	
#include "SDL_video.h"     // Complete for 1.2.11	
#include "SDL_version.h"   // Complete for 1.2.11	
#include "SDL.h"  	   // Complete for 1.2.11	
%}

//Uncomment %feature to generate :exports
//%feature("export");


// function args of type void become pointer  (note this does not work yet)
%typemap(cin) void* ":pointer";

// "SDL_stdinc.h"
//
typedef unsigned char	Uint8;
typedef signed char	Sint8;
typedef unsigned short	Uint16;
typedef signed short	Sint16;
typedef unsigned int	Uint32;
typedef signed int	Sint32;
// end "SDL_stdinc.h"

// From "windows.h", I guess. This needs to be here to support "SDL_syswm.h"
struct HWND__ { int unused; }; 
typedef struct HWND__ *HWND;
struct HGLRC__ { int unused; }; 
typedef struct HGLRC__ *HGLRC;
struct HDC__ { int unused; }; 
typedef struct HDC__ *HDC;

typedef unsigned int WPARAM;
typedef long LPARAM;
// end "windows.h"

// "SDL_video.h"
// The following structure is located in sdlswig.i
%ignore SDL_VideoInfo;
// end "SDL_video.h"

// "SDL_error.h"
%ignore SDL_SetError;
// end "SDL_error.h"

// "SDL_joystick.h"
// The following #define macros are located in "post-swig.lisp"
%ignore SDL_HAT_RIGHTUP;
%ignore SDL_HAT_RIGHTDOWN;
%ignore SDL_HAT_LEFTUP;
%ignore SDL_HAT_LEFTDOWN;
// end "SDL_joystick.h"

// "SDL_keysym.h"
// The following #define macros are located in "post-swig.lisp"
%ignore KMOD_CTRL;
%ignore KMOD_SHIFT;
%ignore KMOD_ALT;
%ignore KMOD_META;
// end "SDL_keysym.h"

// "SDL_video.h"
// The following #define macros are located in "post-swig.lisp"
%ignore SDL_MUSTLOCK;
%ignore SDL_LoadBMP;
%ignore SDL_SaveBMP;
%ignore SDL_BlitSurface;
// end "SDL_video.h"

// "SDL_version.h"
// The following #define macros are located in "post-swig.lisp"
%ignore SDL_VERSION;
%ignore SDL_VERSIONNUM;
%ignore SDL_COMPILEDVERSION;
%ignore SDL_VERSION_ATLEAST;
// end "SDL_version.h"

// "SDL_audio.h"
// The following #define macros are located in "post-swig.lisp"
%ignore AUDIO_U16SYS;
%ignore AUDIO_S16SYS;
%ignore SDL_LoadWAV;
// end "SDL_audio.h"

// "SDL_cdrom.h"
// The following #define is located in "post-swig.lisp"
%ignore CD_INDRIVE;
%ignore CD_FPS;
%ignore FRAMES_TO_MSF;
%ignore MSF_TO_FRAMES;
// end "SDL_cdrom.h"

// "SDL_mouse.h"
// The following #define macros are located in "post-swig.lisp"
%ignore SDL_BUTTON;
%ignore SDL_BUTTON_LMASK;
%ignore SDL_BUTTON_MMASK;
%ignore SDL_BUTTON_RMASK;
// end "SDL_mouse.h"

// "SDL_events.h"
// The following #define macros are located in "post-swig.lisp"
%ignore SDL_EventMask;
%ignore SDL_ACTIVEEVENTMASK;
%ignore SDL_KEYUPMASK;
%ignore SDL_KEYEVENTMASK;
%ignore SDL_MOUSEMOTIONMASK;
%ignore SDL_MOUSEBUTTONDOWNMASK;
%ignore SDL_MOUSEBUTTONUPMASK;
%ignore SDL_MOUSEEVENTMASK;
%ignore SDL_JOYAXISMOTIONMASK;
%ignore SDL_JOYBALLMOTIONMASK;
%ignore SDL_JOYHATMOTIONMASK;
%ignore SDL_JOYBUTTONDOWNMASK;
%ignore SDL_JOYBUTTONUPMASK;
%ignore SDL_JOYEVENTMASK;
%ignore SDL_VIDEORESIZEMASK;
%ignore SDL_VIDEOEXPOSEMASK;
%ignore SDL_JOYEVENTMASK;
%ignore SDL_SYSWMEVENTMASK;
// end "SDL_events.h"


%include "begin_code.h"

%include "SDL_main.h"
%include "SDL_error.h"
%include "SDL_rwops.h"

%include "SDL_audio.h"

%include "SDL_cdrom.h"
%include "SDL_joystick.h"

%include "SDL_active.h"

%include "SDL_keysym.h"
%include "SDL_keyboard.h"

%include "SDL_mouse.h"
%include "SDL_joystick.h"
%include "SDL_quit.h"

%include "SDL_events.h"

%include "SDL_video.h"

%include "SDL_version.h"

%include "SDL.h"
