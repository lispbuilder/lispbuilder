%insert("lisphead") 
%{
;;;; SDL CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2006 Justin Heyes-Jones, Luke J Crook
;;;; See COPYING for license
;;;;
;;;; This .i file has been tested with SDL version 1.2.12
;;;;
;;;; Run SWIG using something like:
;;;;
;;;; 

;; include/*.h changes from SDL-1.2.9 to SDL-1.2.11
;;
;;	SDL-1.2.9		SDL-1.2.11	 
;;	==========		=========	 
;;	SDL.h			SDL.h		 
;;	SDL_active.h		SDL_active.h	 
;;	SDL_audio.h		SDL_audio.h	     
;;	SDL_byteorder.h		SDL_byteorder.h	 // Depreciated. Now in "SDL_stdinc.h"
;;	SDL_cdrom.h		SDL_cdrom.h	     
;;	SDL_copying.h		SDL_copying.h	 
;;				SDL_config.h
;;	SDL_cpuinfo.h		SDL_cpuinfo.h	 
;;	SDL_endian.h		SDL_endian.h	 
;;	SDL_error.h		SDL_error.h	 
;;	SDL_event.h		SDL_events.h	 
;;	SDL_getenv.h		SDL_getenv.h	 // Depreciated. Now in "SDL_stdinc.h"
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
;;	SDL_types.h		SDL_types.h	 // Depreciated. Now in "SDL_stdinc.h"
;;	SDL_version.h		SDL_version.h	 
;;	SDL_video.h		SDL_video.h	 
;;	begin_code.h		begin_code.h	 
;;	close_code.h		close_code.h	 
;;
;; The following #includes are not processed by sdlswig.i
;;  - "SDL_config.h" 	   // Not required for bindings.			// Complete for 1.2.11
;;  - "SDL_syswm.h" 	   // Partially defined in sdlswig.i			// Complete for 1.2.11
;;  - "SDL_stdinc.h" 	   // Partially defined in sdlswig.i. 	    		// Complete for 1.2.11
;;     			   // Only the types and putenv/getenv are required.	
;;  - "SDL_endian.h"  	   // Not required for bindings.       	   		// Complete for 1.2.11
;;  - "SDL_thread.h" 	   // Use native Lisp threads instead. 			// Complete for 1.2.11
;;  - "SDL_mutex.h" 	   // Use native Lisp threads instead. 			// Complete for 1.2.11
;;  - "SDL_opengl.h" 	   // Use CL-OPENGL instead.  	      	 		// Complete for 1.2.11

(in-package #:lispbuilder-sdl-cffi)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'sdl-lispify)
(defun sdl-lispify (name flag &optional (package *package*))
  (labels ((find-sub (src lst)
	     (when (>= (length lst)
		       (length src))
	       (if (equal src (subseq lst 0 (length src)))
		   t
		   nil)))
	   (replace-sub (new old lis)
	     (append new (nthcdr (length old) lis)))
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
		(cond
		  ((null lst)
		   rest)
		  ((upper-case-p c)
		   (let ((new '(#\K #\E #\Y #\_)) (old '(#\S #\D #\L #\K #\_)))
		     (when (find-sub old lst)
		       (setf lst (replace-sub new old lst)
			     c (first new))))
		   (let ((new '(#\K #\E #\Y #\_ #\M #\O #\D #\_)) (old '(#\K #\M #\O #\D #\_)))
		     (when (find-sub old lst)
		       (setf lst (replace-sub new old lst)
			     c (first new))))		   
		   (helper (cdr lst) 'upper last
			   (cond
			     ((or (equal last 'lower)
;; 				  (equal last 'digit)
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
		    ((constant variable) "*")
		    (enumvalue "")
		    (t ""))))
      (intern
       (concatenate
	'string
	fix
	(nreverse (helper (concatenate 'list name) nil nil nil))
	fix)
       package))))
))

;;;; Lispifies the following 'C' keywords:
;;;; scancode 		=    SCANCODE
;;;; SDL_ALL_HOTKEYS 	=    *SDL-ALL-HOTKEYS*
;;;; SDLKey 		=    SDL-KEY
;;;; SDL_GetKeyRepeat	=    SDL-GET-KEY-REPEAT
;;;; SDL_RWFromFP	=    SDL-RW-FROM-FP
;;;; SDL_HasSSE 	=    SDL-HAS-SSE
;;;; SDL_HasSSE2 	=    SDL-HAS-SSE-2
;;;; SDL_Has3DNow 	=    SDL-HAS-3D-NOW
;;;; SDL_WriteBE32	=    SDL-WRITE-BE-32
;;;; SDLK_SLASH		=    :KEY-SLASH
;;;; SDLK_F1		=    :KEY-F-1
;;;; KMOD_LSHIFT	=    :KEY-MOD-LSHIFT


;;; "SDL_audio.h"
#-(or little-endian PC386 X86 I386) (defconstant *AUDIO-U16SYS* #x1010)
#-(or little-endian PC386 X86 I386) (defconstant *AUDIO-U16SYS* #x9010)
#+(or little-endian PC386 X86 I386) (defconstant *AUDIO-U16SYS* #x0010)
#+(or little-endian PC386 X86 I386) (defconstant *AUDIO-S16SYS* #x8010)
;;; End "SDL_audio.h"

;;; "SDL_video.h"
;;; Here we define SDL_VideoInfo as it uses icky nasty bitfields 
;;; that SWIG does not yet generate wrappers for.
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

;;; "SDL_version.h"
(cffi:defcstruct SDL-VERSION
	(major :unsigned-char)
	(minor :unsigned-char)
	(patch :unsigned-char))
;;; end "SDL_version.h"


;;; "SDL_syswm.h"
(cffi:defcstruct HWND__
	(unused :int))

(cffi:defcstruct HGLRC__
	(unused :int))

(cffi:defcstruct HDC__
	(unused :int))

#+win32 (defcstruct SDL-Sys-WM-msg
	(version SDL-version)
	(hwnd :pointer)
	(msg :pointer)
	(wParam :unsigned-int)
	(lParam :long))

#+win32 (defcstruct SDL-Sys-WM-info
	(version SDL-version)
	(window :pointer)
	(hglrc :pointer))

#-win32 (defcenum SDL-SYS-WM-TYPE
	:SDL-SYS-WM-X11)

#-win32 (defcunion SDL-Sys-WM-msg-event
	(xevent :pointer))

#-win32 (defcstruct SDL-Sys-WM-msg
	(version SDL-version)
	(subsystem SDL-SYS-WM-TYPE)
	(event SDL-Sys-WM-msg-event))

#-win32 (defcstruct SDL-Sys-WM-info-info-x11
	(display :pointer)
	(window :unsigned-long)
	(lock-func :pointer)
	(unlock-func :pointer)
	(fswindow :unsigned-long)
	(wmwindow :unsigned-long))

#-win32 (defcunion SDL-Sys-WM-info-info
	(x11 SDL-Sys-WM-info-info-x11))

#-win32 (defcstruct SDL-Sys-WM-info
	(version SDL-version)
	(subsystem SDL-SYS-WM-TYPE)
	(info SDL-Sys-WM-info-info))
;;; end "SDL_syswm.h"
%}

%module sdl

%feature("intern_function","sdl-lispify");

// function args of type void become pointer  (note this does not work yet)
%typemap(cin) void* ":pointer";


// %includes begin from here.
%include "begin_code.h"

// "SDL_stdinc.h"
//
typedef unsigned char	Uint8;
typedef signed char	Sint8;
typedef unsigned short	Uint16;
typedef signed short	Sint16;
typedef unsigned int	Uint32;
typedef signed int	Sint32;

extern char * SDL_getenv(const char *name);
extern int SDL_putenv(const char *variable);
// end "SDL_stdinc.h"

%include "SDL_platform.h"
%include "SDL_timer.h"
%include "SDL_main.h"
%include "SDL_error.h"
%include "SDL_rwops.h"

%ignore AUDIO_U16SYS;	// *AUDIO-U16SYS*. In "lisphead".
%ignore AUDIO_S16SYS;	// *AUDIO-U16SYS*. In "lisphead".
%ignore SDL_LoadWAV;	// SDL-LOAD-WAV. In "lisphead".
%include "SDL_audio.h"

%ignore CD_INDRIVE;	// CD-IN-DRIVE. In "swiglisp".
%ignore FRAMES_TO_MSF;	// FRAMES-TO-MSF. In "swiglisp".
%ignore MSF_TO_FRAMES;	// MSF-TO-FRAMES. In "swiglisp".
%include "SDL_cdrom.h"

%include "SDL_joystick.h"
%include "SDL_active.h"

%ignore KMOD_CTRL;	// *KEY-MOD-CTRL*. In "swiglisp".
%ignore KMOD_SHIFT;	// *KEY-MOD-SHIFT*. In "swiglisp".
%ignore KMOD_ALT;	// *KEY-MOD-ALT*. In "swiglisp".
%ignore KMOD_META;	// *KEY-MOD-META*. In "swiglisp".
%include "SDL_keysym.h"

%include "SDL_keyboard.h"

%ignore SDL_BUTTON;	  // SDL-BUTTON. In "swiglisp".
%ignore SDL_BUTTON_LMASK; // SDL-BUTTON-LMASK. In "swiglisp".
%ignore SDL_BUTTON_MMASK; // SDL-BUTTON-MMASK. In "swiglisp".
%ignore SDL_BUTTON_RMASK; // SDL-BUTTON-RMASK. In "swiglisp".
%include "SDL_mouse.h"

%include "SDL_joystick.h"
%include "SDL_quit.h"

// Add the other ignored events.
// %ignore SDL_EventType;		  // SDL-EVENT-TYPE. In "swiglisp"

%ignore SDL_EventMask;		  // SDL-EVENT-MASK. In "swiglisp".
%ignore SDL_EVENTMASK;		  // SDL-EVENT-MASK. In "swiglisp".
				  // SDL-ACTIVE-EVENT-MASK. In "swiglisp".
				  // SDL-KEY-UP-MASK. In "swiglisp".
				  // SDL-KEY-DOWN-MASK. In "swiglisp".
				  // SDL-KEY-EVENT-MASK. In "swiglisp".
				  // SDL-MOUSE-MOTION-MASK. In "swiglisp".
				  // SDL-MOUSE-BUTTON-DOWN-MASK. In "swiglisp".
				  // SDL-MOUSE-BUTTON-UP-MASK. In "swiglisp".
				  // SDL-MOUSE-EVENT-MASK. In "swiglisp".
				  // SDL-JOY-AXIS-MOTION-MASK. In "swiglisp".
				  // SDL-JOY-BALL-MOTION-MASK. In "swiglisp".
				  // SDL-JOY-HAT-MOTION-MASK. In "swiglisp".
				  // SDL-JOY-BUTTON-DOWN-MASK. In "swiglisp".
				  // SDL-JOY-BUTTON-UP-MASK. In "swiglisp".
				  // SDL-JOY-EVENT-MASK. In "swiglisp".
				  // SDL-JOY-EVENT-MASK. In "swiglisp".
				  // SDL-VIDEO-RESIZE-MASK. In "swiglisp".
				  // SDL-VIDEO-EXPOSE-MASK. In "swiglisp".
				  // SDL-QUIT-MASK. In "swiglisp".
				  // SDL-SYS-WM-EVENT-MASK. In "swiglisp".
%include "SDL_events.h"

%ignore SDL_VideoInfo;	 // In "lisphead".
%ignore SDL_MUSTLOCK;	 // Not defined.
%ignore SDL_LoadBMP;	 // SDL-LOAD-BMP. In "swig.lisp".
%ignore SDL_SaveBMP;	 // SDL-SAVE-BMP. In "swig.lisp".
%ignore SDL_BlitSurface; // SDL-BLIT-SURFACE. In "swig.lisp".
%include "SDL_video.h"	 // SDL-LOAD-BMP. In "swig.lisp". 

%ignore SDL_VERSION;	     // In "lisphead".
%ignore SDL_VERSIONNUM;	     // SDL-VERSION-NUM. In "swiglisp".
%ignore SDL_VERSION_ATLEAST; // SDL-VERSION-AT-LEAST. In "swiglisp".
%include "SDL_version.h"

%include "SDL.h"



%insert("swiglisp") %{
;;;;; Here we place functions and macros that depend on 
;;;;; the bindings generated by SWIG.
;;;;;

;;; "SDL_audio.h"
(defun SDL-LOAD-WAV (file spec audio-buf audio-len)
  (SDL-LOAD-WAV-RW (SDL-RW-FROM-FILE file "rb")
		  1
		  spec
		  audio-buf
		  audio-len))
;;; end "SDL_audio.h"

;;; "SDL_video.h"
(defun SDL-Load-BMP (file)
  (SDL-Load-BMP-RW (SDL-RW-FROM-FILE file "rb") 1))

(defun SDL-Save-BMP (surface file)
  (SDL-Save-BMP-RW surface (SDL-RW-FROM-FILE file "wb") 1))

(defun SDL-Blit-Surface (src srcrect dst dstrect)
  (SDL-Upper-Blit src srcrect dst dstrect))
;;; end "SDL_video.h"

;;; "SDL_version.h"
(defun SDL-VERSION (x)
  (cffi:with-foreign-slots ((major minor patch) x SDL-version)
    (setf major SDL-MAJOR-VERSION
          minor SDL-MINOR-VERSION
          patch SDL-PATCH-LEVEL)))

(defun SDL-VERSION-NUM (major minor patch)
        (+  (* major 1000)
            (* minor 100)
            patch))

(defun SDL-VERSION-AT-LEAST (x y z)
  (if (>= *SDL-COMPILEDVERSION*	 (SDL-VERSION-NUM x y z))
      1
      0))
;;; end "SDL_version.h"

;;; "SDL_syswm.h"
(defcfun ("SDL_GetWMInfo" SDL-Get-WM-Info) :int
  (info :pointer))
;;; end "SDL_syswm.h"

;;; "SDL_cdrom.h"
(defun CD-IN-DRIVE	 (status)
  (if (> status 0)
      t
    nil))

(defun FRAMES-TO-MSF (f)
  (values 
   (mod f CD-FPS)
   (mod (/ f CD-FPS) 60)
   (/ (/ f CD-FPS) 60)))

(defun MSF-TO-FRAMES (M S F)
  (+ 
   (* M 60 CD-FPS)
   (* S CD-FPS)
   F))
;;; end "SDL_cdrom.h"

;;; "SDL_mouse.h"
(defun SDL-BUTTON (X)
  (1<< (- X 1)))

(defun SDL-BUTTON-LMASK ()
  (SDL-BUTTON SDL-BUTTON-LEFT))

(defun SDL-BUTTON-MMASK ()
  (SDL-BUTTON SDL-BUTTON-MIDDLE))

(defun SDL-BUTTON-RMASK ()
  (SDL-BUTTON SDL-BUTTON-RIGHT))
;;; end "SDL_mouse.h"

;;; "SDL_keysym"
(defconstant *KEY-MOD-CTRL*	(logior  (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-LCTRL)
					 (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-RCTRL)))
(defconstant *KEY-MOD-SHIFT*	(logior  (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-LSHIFT)
					 (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-RSHIFT)))
(defconstant *KEY-MOD-ALT*	(logior  (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-LALT)
					 (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-RALT)))
(defconstant *KEY-MOD-META*	(logior  (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-LMETA)
					 (cffi:foreign-enum-value 'Sdl-Mod :KEY-MOD-RMETA)))
;;; end "SDL_keysym.h"

;;; "SDL_events.h"
(defun SDL-EVENT-MASK (X)
  (1<< X))

(defun SDL-ACTIVE-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-ACTIVEEVENT)))

(defun SDL-KEY-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEYDOWN)))

(defun SDL-KEY-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEYUP)))

(defun SDL-KEY-EVENT-MASK ()
  (or (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEYUP))
      (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-KEYDOWN))))

(defun SDL-MOUSE-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSE-MOTION)))

(defun SDL-MOUSE-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSEBUTTONDOWN)))

(defun SDL-MOUSE-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSEBUTTONUP)))

(defun SDL-MOUSE-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSEMOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSEBUTTONDOWN))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-MOUSEBUTTONUP))))

(defun SDL-JOY-AXIS-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYAXISMOTION)))

(defun SDL-JOY-BALL-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBALLMOTION)))

(defun SDL-JOY-HAT-MOTION-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYHATMOTION)))

(defun SDL-JOY-BUTTON-DOWN-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBUTTONDOWN)))

(defun SDL-JOY-BUTTON-UP-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBUTTONUP)))

(defun SDL-JOY-EVENT-MASK ()
  (logior (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYMOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBALLMOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYHATMOTION))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBUTTONDOWN))
          (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-JOYBUTTONUP))))

(defun SDL-VIDEO-RESIZE-MASK () 
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEORESIZE)))

(defun SDL-VIDEO-EXPOSE-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-VIDEOEXPOSE)))

(defun SDL-QUIT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-QUIT)))

(defun SDL-SYS-WM-EVENT-MASK ()
  (SDL-EVENT-MASK (cffi:foreign-enum-value 'Sdl-Event-Type :SDL-SYSWMEVENT)))
;;; end "SDL_events.h"
%}