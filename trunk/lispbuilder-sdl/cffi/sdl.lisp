
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

(in-package #:lispbuilder-sdl-cffi)

(cl:defconstant SDL-INIT-TIMER #x00000001)
(cl:defconstant SDL-INIT-AUDIO #x00000010)
(cl:defconstant SDL-INIT-VIDEO #x00000020)
(cl:defconstant SDL-INIT-CDROM #x00000100)
(cl:defconstant SDL-INIT-JOYSTICK #x00000200)
(cl:defconstant SDL-INIT-NOPARACHUTE #x00100000)
(cl:defconstant SDL-INIT-EVENTTHREAD #x01000000)
(cl:defconstant SDL-INIT-EVERYTHING #x0000FFFF)

(cffi:defcfun ("SDL_Init" SDL-Init) :int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_InitSubSystem" SDL-Init-SubSystem) :int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_QuitSubSystem" SDL-Quit-SubSystem) :void
  (flags :unsigned-int))

(cffi:defcfun ("SDL_WasInit" SDL-Was-Init) :unsigned-int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_Quit" SDL-Quit) :void)
