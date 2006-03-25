%insert("lisphead") 
%{
;;;; SDL CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Justin Heyes-Jones
;;;; See COPYING for license
;;;;
;;;; This .i file has been tested with SDL version 1.2.9

(in-package #:lispbuilder-sdl)

;;;; First, set the byte-order: "SDL_byteorder.h"
(defconstant SDL_LIL_ENDIAN 1234)
(defconstant SDL_BIG_ENDIAN 4321)

;; Set the byte order for the current CPU
#-(or little-endian PC386 X86 I386) (defconstant SDL_BYTEORDER SDL_BIG_ENDIAN)
#+(or little-endian PC386 X86 I386) (defconstant SDL_BYTEORDER SDL_LIL_ENDIAN)
;;;; End

;;;; the SDL_Event is a union which doesn't work yet with swig
;;;; todo need some way to handle this better
(defcstruct SDL_Event
	(type :unsigned-char)
	(pada :int)	
	(padb :int)	
	(padc :int)	
	(padd :int)	
	(pade :int)	
	(padf :int)	
	(padg :int))

;;;; SDL_VideoInfo uses nasty bitfields. CFFI does not yet support these.
(defcstruct SDL_VideoInfo
  (int1 :unsigned-char)
  (int2 :unsigned-char)
  (int3 :unsigned-short)
  (video_mem :unsigned-int)
  (vfmt :pointer))

;;;; SDL_keysym is redefined here as CFFI treats 'sym' and 'mod' as pointers and not enums.
(defcstruct SDL_keysym
  (scancode :unsigned-char)
  (sym :int)
  (mod :int)
  (unicode :unsigned-short))


;;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

;;;; Macro to handle defenum (thanks to Frank Buss for this SWIG/CFFI feature
; this handles anonymous enum types differently

(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))


;;;; "SDL_types.h"
(defcenum SDL_bool
	(:SDL_FALSE 0)
	(:SDL_TRUE 1))

(defcstruct Uint64
	(hi :unsigned-int)
	(lo :unsigned-int))

(defcenum SDL_DUMMY_ENUM
	:DUMMY_ENUM_VALUE)

(defconstant SDL_PRESSED  #x01)
(defconstant SDL_RELEASED #x00)
;;;; end "SDL_types.h"

;;;; "SDL_video.h"
(defcfun ("SDL_GL_SetAttribute" SDL_GL_SetAttribute) :int
  (attr :int)
  (value :int))

(defcfun ("SDL_GL_GetAttribute" SDL_GL_GetAttribute) :int
  (attr :int)
  (value :pointer))
;;;; end "SDL_video.h"

;;;; "SDL_syswm.h"
#+win32 (defcstruct SDL_SysWMmsg
	(version :pointer)
	(hwnd :pointer)
	(msg :pointer)
	(wParam :unsigned-int)
	(lParam :long))

#+win32 (defcstruct SDL_SysWMinfo
	(version :pointer)
	(window :pointer)
	(hglrc :pointer))

#-win32 (defcenum SDL_SYSWM_TYPE
	:SDL_SYSWM_X11)

#-win32 (defcstruct SDL_SysWMmsg
	(version :pointer)
	(subsystem :pointer)
	(event :pointer))

#-win32 (defcunion SDL_SysWMmsg_event
	(xevent :pointer))

#-win32 (defcstruct SDL_SysWMinfo
	(version :pointer)
	(subsystem :pointer)
	(info :pointer))

#-win32 (defcunion SDL_SysWMinfo_info
	(x11 :pointer))

#-win32 (defcstruct SDL_SysWMinfo_info_x11
	(display :pointer)
	(window :pointer)
	(lock_func :pointer)
	(unlock_func :pointer)
	(fswindow :pointer)
	(wmwindow :pointer))

(defcfun ("SDL_GetWMInfo" SDL_GetWMInfo) :int
  (info :pointer))
;;;; end "SDL_syswm.h"

%}

%module sdl
%{

#include "begin_code.h"
//#include "SDL_types.h" // Removed. Weird #define breaks SWIG on line 90. Necessary types are defined above.

#include "SDL_main.h"
#include "SDL_getenv.h"
#include "SDL_error.h"
#include "SDL_rwops.h"
#include "SDL_timer.h"
#include "SDL_audio.h"
#include "SDL_cdrom.h"
#include "SDL_joystick.h"

#include "SDL_active.h"

#include "SDL_keysym.h"
#include "SDL_keyboard.h"

#include "SDL_mouse.h"
#include "SDL_joystick.h"
#include "SDL_quit.h"

#include "SDL_events.h"

#include "SDL_mutex.h"
#include "SDL_video.h"

//#include "SDL_byteorder.h"  // Not needed.
#include "SDL_version.h"

#include "SDL.h"

//#include "SDL_syswm.h" // Removed. Defined above.

%}

// function args of type void become pointer  (note this does not work yet)
%typemap(cin) void* ":pointer";

// "SDL_types.h"
typedef unsigned char	Uint8;
typedef signed char	Sint8;
typedef unsigned short	Uint16;
typedef signed short	Sint16;
typedef unsigned int	Uint32;
typedef signed int	Sint32;
// end "SDL_types.h"

struct HWND__ { int unused; }; 
typedef struct HWND__ *HWND;
struct HGLRC__ { int unused; }; 
typedef struct HGLRC__ *HGLRC;
struct HDC__ { int unused; }; 
typedef struct HDC__ *HDC;

typedef unsigned int WPARAM;
typedef long LPARAM;



%ignore SDL_Event;
%ignore SDL_VideoInfo;
%ignore SDL_keysym;
%ignore SDL_SetError;
%ignore SDL_HAT_RIGHTUP;
%ignore SDL_HAT_RIGHTDOWN;
%ignore SDL_HAT_LEFTUP;
%ignore SDL_HAT_LEFTDOWN;
%ignore KMOD_CTRL;
%ignore KMOD_SHIFT;
%ignore KMOD_ALT;
%ignore KMOD_META;
%ignore SDL_MUTEX_MAXWAIT;
%ignore SDL_MUSTLOCK;
%ignore SDL_VERSION;
%ignore SDL_VERSIONNUM;
%ignore SDL_COMPILEDVERSION;
%ignore SDL_VERSION_ATLEAST;
%ignore AUDIO_U16SYS;
%ignore AUDIO_S16SYS;
%ignore CD_FPS;
%ignore SDL_GL_SetAttribute;
%ignore SDL_GL_GetAttribute;


%include "begin_code.h"
//%include "SDL_types.h"

%include "SDL_main.h"
%include "SDL_getenv.h"
%include "SDL_error.h"
%include "SDL_rwops.h"
%include "SDL_timer.h"
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

%include "SDL_mutex.h"
%include "SDL_video.h"

//%include "SDL_byteorder.h" // Not needed. Defined above.
%include "SDL_version.h"

%include "SDL.h"

//%include "SDL_syswm.h" //Removed. Defined above.

