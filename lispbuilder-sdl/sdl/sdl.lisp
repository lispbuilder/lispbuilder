
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
  (:hw-available  #x0001)
  (:wm-available  #x0002)
  (:unused_bits_1 #x0004)
  (:unused_bits_2 #x0008)
  (:unused_bits_3 #x0010)  
  (:unused_bits_4 #x0020)
  (:unused_bits_5 #x0040)
  (:unused_bits_6 #x0080)
  (:unused_bits_7 #x0100)
  (:blit-hw       #x0200)
  (:blit-hw-CC    #x0400)
  (:blit-hw-A     #x0800)
  (:blit-sw       #x1000)
  (:blit-sw-CC    #x2000)
  (:blit-sw-A     #x4000)
  (:blit-fill     #x8000))

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
(defcfun ("SDL_GetTicks" SDL_GetTicks) :int)

;;/* Wait a specified number of milliseconds before returning */
;;extern DECLSPEC void SDLCALL SDL_Delay(Uint32 ms);
(defcfun ("SDL_Delay" SDL_Delay) :void
  (ms :int))
;;;; end "SDL_timer.h"


;;;;
;;;; end Overrides



;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(cffi:defcstruct HWND__
	(unused :int))

(cffi:defcstruct HGLRC__
	(unused :int))

(cffi:defcstruct HDC__
	(unused :int))

(cffi:defcfun ("SDL_GetError" SDL_GetError) :string)

(cffi:defcfun ("SDL_ClearError" SDL_ClearError) :void)

(cffi:defcenum SDL_errorcode
	:SDL_ENOMEM
	:SDL_EFREAD
	:SDL_EFWRITE
	:SDL_EFSEEK
	:SDL_UNSUPPORTED
	:SDL_LASTERROR)

(cffi:defcfun ("SDL_Error" SDL_Error) :void
  (code SDL_errorcode))

(cffi:defcstruct SDL_RWops
	(seek :pointer)
	(read :pointer)
	(write :pointer)
	(close :pointer)
	(type :unsigned-int)
	(hidden :pointer))

(cffi:defcunion SDL_RWops_hidden
	(unknown :pointer)
	(mem :pointer))

(cffi:defcstruct SDL_RWops_hidden_unknown
	(data1 :pointer))

(cffi:defcstruct SDL_RWops_hidden_mem
	(base :pointer)
	(here :pointer)
	(stop :pointer))

(cffi:defcfun ("SDL_RWFromFile" SDL_RWFromFile) :pointer
  (file :string)
  (mode :string))

(cffi:defcfun ("SDL_RWFromMem" SDL_RWFromMem) :pointer
  (mem :pointer)
  (size :int))

(cffi:defcfun ("SDL_RWFromConstMem" SDL_RWFromConstMem) :pointer
  (mem :pointer)
  (size :int))

(cffi:defcfun ("SDL_AllocRW" SDL_AllocRW) :pointer)

(cffi:defcfun ("SDL_FreeRW" SDL_FreeRW) :void
  (area :pointer))

(cl:defconstant RW_SEEK_SET 0)

(cl:defconstant RW_SEEK_CUR 1)

(cl:defconstant RW_SEEK_END 2)

(cffi:defcfun ("SDL_ReadLE16" SDL_ReadLE16) :unsigned-short
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE16" SDL_ReadBE16) :unsigned-short
  (src :pointer))

(cffi:defcfun ("SDL_ReadLE32" SDL_ReadLE32) :unsigned-int
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE32" SDL_ReadBE32) :unsigned-int
  (src :pointer))

(cffi:defcfun ("SDL_ReadLE64" SDL_ReadLE64) :pointer
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE64" SDL_ReadBE64) :pointer
  (src :pointer))

(cffi:defcfun ("SDL_WriteLE16" SDL_WriteLE16) :int
  (dst :pointer)
  (value :unsigned-short))

(cffi:defcfun ("SDL_WriteBE16" SDL_WriteBE16) :int
  (dst :pointer)
  (value :unsigned-short))

(cffi:defcfun ("SDL_WriteLE32" SDL_WriteLE32) :int
  (dst :pointer)
  (value :unsigned-int))

(cffi:defcfun ("SDL_WriteBE32" SDL_WriteBE32) :int
  (dst :pointer)
  (value :unsigned-int))

(cffi:defcfun ("SDL_WriteLE64" SDL_WriteLE64) :int
  (dst :pointer)
  (value :pointer))

(cffi:defcfun ("SDL_WriteBE64" SDL_WriteBE64) :int
  (dst :pointer)
  (value :pointer))

(cffi:defcstruct SDL_AudioSpec
	(freq :int)
	(format :unsigned-short)
	(channels :unsigned-char)
	(silence :unsigned-char)
	(samples :unsigned-short)
	(padding :unsigned-short)
	(size :unsigned-int)
	(callback :pointer)
	(userdata :pointer))

(cl:defconstant AUDIO_U8 #x0008)

(cl:defconstant AUDIO_S8 #x8008)

(cl:defconstant AUDIO_U16LSB #x0010)

(cl:defconstant AUDIO_S16LSB #x8010)

(cl:defconstant AUDIO_U16MSB #x1010)

(cl:defconstant AUDIO_S16MSB #x9010)

(cl:defconstant AUDIO_U16 #x0010)

(cl:defconstant AUDIO_S16 #x8010)

(cffi:defcstruct SDL_AudioCVT
	(needed :int)
	(src_format :unsigned-short)
	(dst_format :unsigned-short)
	(rate_incr :double)
	(buf :pointer)
	(len :int)
	(len_cvt :int)
	(len_mult :int)
	(len_ratio :double)
	(filters :pointer)
	(filter_index :int))

(cffi:defcfun ("SDL_AudioInit" SDL_AudioInit) :int
  (driver_name :string))

(cffi:defcfun ("SDL_AudioQuit" SDL_AudioQuit) :void)

(cffi:defcfun ("SDL_AudioDriverName" SDL_AudioDriverName) :string
  (namebuf :string)
  (maxlen :int))

(cffi:defcfun ("SDL_OpenAudio" SDL_OpenAudio) :int
  (desired :pointer)
  (obtained :pointer))

(cffi:defcenum SDL_audiostatus
	(:SDL_AUDIO_STOPPED 0)
	:SDL_AUDIO_PLAYING
	:SDL_AUDIO_PAUSED)

(cffi:defcfun ("SDL_GetAudioStatus" SDL_GetAudioStatus) SDL_audiostatus)

(cffi:defcfun ("SDL_PauseAudio" SDL_PauseAudio) :void
  (pause_on :int))

(cffi:defcfun ("SDL_LoadWAV_RW" SDL_LoadWAV_RW) :pointer
  (src :pointer)
  (freesrc :int)
  (spec :pointer)
  (audio_buf :pointer)
  (audio_len :pointer))

(cffi:defcfun ("SDL_FreeWAV" SDL_FreeWAV) :void
  (audio_buf :pointer))

(cffi:defcfun ("SDL_BuildAudioCVT" SDL_BuildAudioCVT) :int
  (cvt :pointer)
  (src_format :unsigned-short)
  (src_channels :unsigned-char)
  (src_rate :int)
  (dst_format :unsigned-short)
  (dst_channels :unsigned-char)
  (dst_rate :int))

(cffi:defcfun ("SDL_ConvertAudio" SDL_ConvertAudio) :int
  (cvt :pointer))

(cl:defconstant SDL_MIX_MAXVOLUME 128)

(cffi:defcfun ("SDL_MixAudio" SDL_MixAudio) :void
  (dst :pointer)
  (src :pointer)
  (len :unsigned-int)
  (volume :int))

(cffi:defcfun ("SDL_LockAudio" SDL_LockAudio) :void)

(cffi:defcfun ("SDL_UnlockAudio" SDL_UnlockAudio) :void)

(cffi:defcfun ("SDL_CloseAudio" SDL_CloseAudio) :void)

(cl:defconstant SDL_MAX_TRACKS 99)

(cl:defconstant SDL_AUDIO_TRACK #x00)

(cl:defconstant SDL_DATA_TRACK #x04)

(cffi:defcenum CDstatus
	:CD_TRAYEMPTY
	:CD_STOPPED
	:CD_PLAYING
	:CD_PAUSED
	(:CD_ERROR -1))

(cffi:defcstruct SDL_CDtrack
	(id :unsigned-char)
	(type :unsigned-char)
	(unused :unsigned-short)
	(length :unsigned-int)
	(offset :unsigned-int))

(cffi:defcstruct SDL_CD
	(id :int)
	(status CDstatus)
	(numtracks :int)
	(cur_track :int)
	(cur_frame :int)
	(track :pointer))

(cffi:defcfun ("SDL_CDNumDrives" SDL_CDNumDrives) :int)

(cffi:defcfun ("SDL_CDName" SDL_CDName) :string
  (drive :int))

(cffi:defcfun ("SDL_CDOpen" SDL_CDOpen) :pointer
  (drive :int))

(cffi:defcfun ("SDL_CDStatus" SDL_CDStatus) CDstatus
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDPlayTracks" SDL_CDPlayTracks) :int
  (cdrom :pointer)
  (start_track :int)
  (start_frame :int)
  (ntracks :int)
  (nframes :int))

(cffi:defcfun ("SDL_CDPlay" SDL_CDPlay) :int
  (cdrom :pointer)
  (start :int)
  (length :int))

(cffi:defcfun ("SDL_CDPause" SDL_CDPause) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDResume" SDL_CDResume) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDStop" SDL_CDStop) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDEject" SDL_CDEject) :int
  (cdrom :pointer))

(cffi:defcfun ("SDL_CDClose" SDL_CDClose) :void
  (cdrom :pointer))

(cffi:defcfun ("SDL_NumJoysticks" SDL_NumJoysticks) :int)

(cffi:defcfun ("SDL_JoystickName" SDL_JoystickName) :string
  (device_index :int))

(cffi:defcfun ("SDL_JoystickOpen" SDL_JoystickOpen) :pointer
  (device_index :int))

(cffi:defcfun ("SDL_JoystickOpened" SDL_JoystickOpened) :int
  (device_index :int))

(cffi:defcfun ("SDL_JoystickIndex" SDL_JoystickIndex) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumAxes" SDL_JoystickNumAxes) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumBalls" SDL_JoystickNumBalls) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumHats" SDL_JoystickNumHats) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickNumButtons" SDL_JoystickNumButtons) :int
  (joystick :pointer))

(cffi:defcfun ("SDL_JoystickUpdate" SDL_JoystickUpdate) :void)

(cffi:defcfun ("SDL_JoystickEventState" SDL_JoystickEventState) :int
  (state :int))

(cffi:defcfun ("SDL_JoystickGetAxis" SDL_JoystickGetAxis) :short
  (joystick :pointer)
  (axis :int))

(cl:defconstant SDL_HAT_CENTERED #x00)

(cl:defconstant SDL_HAT_UP #x01)

(cl:defconstant SDL_HAT_RIGHT #x02)

(cl:defconstant SDL_HAT_DOWN #x04)

(cl:defconstant SDL_HAT_LEFT #x08)

(cffi:defcfun ("SDL_JoystickGetHat" SDL_JoystickGetHat) :unsigned-char
  (joystick :pointer)
  (hat :int))

(cffi:defcfun ("SDL_JoystickGetBall" SDL_JoystickGetBall) :int
  (joystick :pointer)
  (ball :int)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("SDL_JoystickGetButton" SDL_JoystickGetButton) :unsigned-char
  (joystick :pointer)
  (button :int))

(cffi:defcfun ("SDL_JoystickClose" SDL_JoystickClose) :void
  (joystick :pointer))

(cl:defconstant SDL_APPMOUSEFOCUS #x01)

(cl:defconstant SDL_APPINPUTFOCUS #x02)

(cl:defconstant SDL_APPACTIVE #x04)

(cffi:defcfun ("SDL_GetAppState" SDL_GetAppState) :unsigned-char)

(cffi:defcenum SDLKey
	(:SDLK_UNKNOWN 0)
	(:SDLK_FIRST 0)
	(:SDLK_BACKSPACE 8)
	(:SDLK_TAB 9)
	(:SDLK_CLEAR 12)
	(:SDLK_RETURN 13)
	(:SDLK_PAUSE 19)
	(:SDLK_ESCAPE 27)
	(:SDLK_SPACE 32)
	(:SDLK_EXCLAIM 33)
	(:SDLK_QUOTEDBL 34)
	(:SDLK_HASH 35)
	(:SDLK_DOLLAR 36)
	(:SDLK_AMPERSAND 38)
	(:SDLK_QUOTE 39)
	(:SDLK_LEFTPAREN 40)
	(:SDLK_RIGHTPAREN 41)
	(:SDLK_ASTERISK 42)
	(:SDLK_PLUS 43)
	(:SDLK_COMMA 44)
	(:SDLK_MINUS 45)
	(:SDLK_PERIOD 46)
	(:SDLK_SLASH 47)
	(:SDLK_0 48)
	(:SDLK_1 49)
	(:SDLK_2 50)
	(:SDLK_3 51)
	(:SDLK_4 52)
	(:SDLK_5 53)
	(:SDLK_6 54)
	(:SDLK_7 55)
	(:SDLK_8 56)
	(:SDLK_9 57)
	(:SDLK_COLON 58)
	(:SDLK_SEMICOLON 59)
	(:SDLK_LESS 60)
	(:SDLK_EQUALS 61)
	(:SDLK_GREATER 62)
	(:SDLK_QUESTION 63)
	(:SDLK_AT 64)
	(:SDLK_LEFTBRACKET 91)
	(:SDLK_BACKSLASH 92)
	(:SDLK_RIGHTBRACKET 93)
	(:SDLK_CARET 94)
	(:SDLK_UNDERSCORE 95)
	(:SDLK_BACKQUOTE 96)
	(:SDLK_a 97)
	(:SDLK_b 98)
	(:SDLK_c 99)
	(:SDLK_d 100)
	(:SDLK_e 101)
	(:SDLK_f 102)
	(:SDLK_g 103)
	(:SDLK_h 104)
	(:SDLK_i 105)
	(:SDLK_j 106)
	(:SDLK_k 107)
	(:SDLK_l 108)
	(:SDLK_m 109)
	(:SDLK_n 110)
	(:SDLK_o 111)
	(:SDLK_p 112)
	(:SDLK_q 113)
	(:SDLK_r 114)
	(:SDLK_s 115)
	(:SDLK_t 116)
	(:SDLK_u 117)
	(:SDLK_v 118)
	(:SDLK_w 119)
	(:SDLK_x 120)
	(:SDLK_y 121)
	(:SDLK_z 122)
	(:SDLK_DELETE 127)
	(:SDLK_WORLD_0 160)
	(:SDLK_WORLD_1 161)
	(:SDLK_WORLD_2 162)
	(:SDLK_WORLD_3 163)
	(:SDLK_WORLD_4 164)
	(:SDLK_WORLD_5 165)
	(:SDLK_WORLD_6 166)
	(:SDLK_WORLD_7 167)
	(:SDLK_WORLD_8 168)
	(:SDLK_WORLD_9 169)
	(:SDLK_WORLD_10 170)
	(:SDLK_WORLD_11 171)
	(:SDLK_WORLD_12 172)
	(:SDLK_WORLD_13 173)
	(:SDLK_WORLD_14 174)
	(:SDLK_WORLD_15 175)
	(:SDLK_WORLD_16 176)
	(:SDLK_WORLD_17 177)
	(:SDLK_WORLD_18 178)
	(:SDLK_WORLD_19 179)
	(:SDLK_WORLD_20 180)
	(:SDLK_WORLD_21 181)
	(:SDLK_WORLD_22 182)
	(:SDLK_WORLD_23 183)
	(:SDLK_WORLD_24 184)
	(:SDLK_WORLD_25 185)
	(:SDLK_WORLD_26 186)
	(:SDLK_WORLD_27 187)
	(:SDLK_WORLD_28 188)
	(:SDLK_WORLD_29 189)
	(:SDLK_WORLD_30 190)
	(:SDLK_WORLD_31 191)
	(:SDLK_WORLD_32 192)
	(:SDLK_WORLD_33 193)
	(:SDLK_WORLD_34 194)
	(:SDLK_WORLD_35 195)
	(:SDLK_WORLD_36 196)
	(:SDLK_WORLD_37 197)
	(:SDLK_WORLD_38 198)
	(:SDLK_WORLD_39 199)
	(:SDLK_WORLD_40 200)
	(:SDLK_WORLD_41 201)
	(:SDLK_WORLD_42 202)
	(:SDLK_WORLD_43 203)
	(:SDLK_WORLD_44 204)
	(:SDLK_WORLD_45 205)
	(:SDLK_WORLD_46 206)
	(:SDLK_WORLD_47 207)
	(:SDLK_WORLD_48 208)
	(:SDLK_WORLD_49 209)
	(:SDLK_WORLD_50 210)
	(:SDLK_WORLD_51 211)
	(:SDLK_WORLD_52 212)
	(:SDLK_WORLD_53 213)
	(:SDLK_WORLD_54 214)
	(:SDLK_WORLD_55 215)
	(:SDLK_WORLD_56 216)
	(:SDLK_WORLD_57 217)
	(:SDLK_WORLD_58 218)
	(:SDLK_WORLD_59 219)
	(:SDLK_WORLD_60 220)
	(:SDLK_WORLD_61 221)
	(:SDLK_WORLD_62 222)
	(:SDLK_WORLD_63 223)
	(:SDLK_WORLD_64 224)
	(:SDLK_WORLD_65 225)
	(:SDLK_WORLD_66 226)
	(:SDLK_WORLD_67 227)
	(:SDLK_WORLD_68 228)
	(:SDLK_WORLD_69 229)
	(:SDLK_WORLD_70 230)
	(:SDLK_WORLD_71 231)
	(:SDLK_WORLD_72 232)
	(:SDLK_WORLD_73 233)
	(:SDLK_WORLD_74 234)
	(:SDLK_WORLD_75 235)
	(:SDLK_WORLD_76 236)
	(:SDLK_WORLD_77 237)
	(:SDLK_WORLD_78 238)
	(:SDLK_WORLD_79 239)
	(:SDLK_WORLD_80 240)
	(:SDLK_WORLD_81 241)
	(:SDLK_WORLD_82 242)
	(:SDLK_WORLD_83 243)
	(:SDLK_WORLD_84 244)
	(:SDLK_WORLD_85 245)
	(:SDLK_WORLD_86 246)
	(:SDLK_WORLD_87 247)
	(:SDLK_WORLD_88 248)
	(:SDLK_WORLD_89 249)
	(:SDLK_WORLD_90 250)
	(:SDLK_WORLD_91 251)
	(:SDLK_WORLD_92 252)
	(:SDLK_WORLD_93 253)
	(:SDLK_WORLD_94 254)
	(:SDLK_WORLD_95 255)
	(:SDLK_KP0 256)
	(:SDLK_KP1 257)
	(:SDLK_KP2 258)
	(:SDLK_KP3 259)
	(:SDLK_KP4 260)
	(:SDLK_KP5 261)
	(:SDLK_KP6 262)
	(:SDLK_KP7 263)
	(:SDLK_KP8 264)
	(:SDLK_KP9 265)
	(:SDLK_KP_PERIOD 266)
	(:SDLK_KP_DIVIDE 267)
	(:SDLK_KP_MULTIPLY 268)
	(:SDLK_KP_MINUS 269)
	(:SDLK_KP_PLUS 270)
	(:SDLK_KP_ENTER 271)
	(:SDLK_KP_EQUALS 272)
	(:SDLK_UP 273)
	(:SDLK_DOWN 274)
	(:SDLK_RIGHT 275)
	(:SDLK_LEFT 276)
	(:SDLK_INSERT 277)
	(:SDLK_HOME 278)
	(:SDLK_END 279)
	(:SDLK_PAGEUP 280)
	(:SDLK_PAGEDOWN 281)
	(:SDLK_F1 282)
	(:SDLK_F2 283)
	(:SDLK_F3 284)
	(:SDLK_F4 285)
	(:SDLK_F5 286)
	(:SDLK_F6 287)
	(:SDLK_F7 288)
	(:SDLK_F8 289)
	(:SDLK_F9 290)
	(:SDLK_F10 291)
	(:SDLK_F11 292)
	(:SDLK_F12 293)
	(:SDLK_F13 294)
	(:SDLK_F14 295)
	(:SDLK_F15 296)
	(:SDLK_NUMLOCK 300)
	(:SDLK_CAPSLOCK 301)
	(:SDLK_SCROLLOCK 302)
	(:SDLK_RSHIFT 303)
	(:SDLK_LSHIFT 304)
	(:SDLK_RCTRL 305)
	(:SDLK_LCTRL 306)
	(:SDLK_RALT 307)
	(:SDLK_LALT 308)
	(:SDLK_RMETA 309)
	(:SDLK_LMETA 310)
	(:SDLK_LSUPER 311)
	(:SDLK_RSUPER 312)
	(:SDLK_MODE 313)
	(:SDLK_COMPOSE 314)
	(:SDLK_HELP 315)
	(:SDLK_PRINT 316)
	(:SDLK_SYSREQ 317)
	(:SDLK_BREAK 318)
	(:SDLK_MENU 319)
	(:SDLK_POWER 320)
	(:SDLK_EURO 321)
	(:SDLK_UNDO 322)
	:SDLK_LAST)

(cffi:defcenum SDLMod
	(:KMOD_NONE #x0000)
	(:KMOD_LSHIFT #x0001)
	(:KMOD_RSHIFT #x0002)
	(:KMOD_LCTRL #x0040)
	(:KMOD_RCTRL #x0080)
	(:KMOD_LALT #x0100)
	(:KMOD_RALT #x0200)
	(:KMOD_LMETA #x0400)
	(:KMOD_RMETA #x0800)
	(:KMOD_NUM #x1000)
	(:KMOD_CAPS #x2000)
	(:KMOD_MODE #x4000)
	(:KMOD_RESERVED #x8000))

(cffi:defcstruct SDL_keysym
	(scancode :unsigned-char)
	(sym SDLKey)
	(mod SDLMod)
	(unicode :unsigned-short))

(cl:defconstant SDL_ALL_HOTKEYS #xFFFFFFFF)

(cffi:defcfun ("SDL_EnableUNICODE" SDL_EnableUNICODE) :int
  (enable :int))

(cl:defconstant SDL_DEFAULT_REPEAT_DELAY 500)

(cl:defconstant SDL_DEFAULT_REPEAT_INTERVAL 30)

(cffi:defcfun ("SDL_EnableKeyRepeat" SDL_EnableKeyRepeat) :int
  (delay :int)
  (interval :int))

(cffi:defcfun ("SDL_GetKeyRepeat" SDL_GetKeyRepeat) :void
  (delay :pointer)
  (interval :pointer))

(cffi:defcfun ("SDL_GetKeyState" SDL_GetKeyState) :pointer
  (numkeys :pointer))

(cffi:defcfun ("SDL_GetModState" SDL_GetModState) SDLMod)

(cffi:defcfun ("SDL_SetModState" SDL_SetModState) :void
  (modstate SDLMod))

(cffi:defcfun ("SDL_GetKeyName" SDL_GetKeyName) :string
  (key SDLKey))

(cffi:defcstruct SDL_Cursor
	(area :pointer)
	(hot_x :short)
	(hot_y :short)
	(data :pointer)
	(mask :pointer)
	(save :pointer)
	(wm_cursor :pointer))

(cffi:defcfun ("SDL_GetMouseState" SDL_GetMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_GetRelativeMouseState" SDL_GetRelativeMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("SDL_WarpMouse" SDL_WarpMouse) :void
  (x :unsigned-short)
  (y :unsigned-short))

(cffi:defcfun ("SDL_CreateCursor" SDL_CreateCursor) :pointer
  (data :pointer)
  (mask :pointer)
  (w :int)
  (h :int)
  (hot_x :int)
  (hot_y :int))

(cffi:defcfun ("SDL_SetCursor" SDL_SetCursor) :void
  (cursor :pointer))

(cffi:defcfun ("SDL_GetCursor" SDL_GetCursor) :pointer)

(cffi:defcfun ("SDL_FreeCursor" SDL_FreeCursor) :void
  (cursor :pointer))

(cffi:defcfun ("SDL_ShowCursor" SDL_ShowCursor) :int
  (toggle :int))

(cl:defconstant SDL_BUTTON_LEFT 1)

(cl:defconstant SDL_BUTTON_MIDDLE 2)

(cl:defconstant SDL_BUTTON_RIGHT 3)

(cl:defconstant SDL_BUTTON_WHEELUP 4)

(cl:defconstant SDL_BUTTON_WHEELDOWN 5)

(cl:defconstant SDL_RELEASED 0)

(cl:defconstant SDL_PRESSED 1)

(cffi:defcenum SDL_EventType
	(:SDL_NOEVENT 0)
	:SDL_ACTIVEEVENT
	:SDL_KEYDOWN
	:SDL_KEYUP
	:SDL_MOUSEMOTION
	:SDL_MOUSEBUTTONDOWN
	:SDL_MOUSEBUTTONUP
	:SDL_JOYAXISMOTION
	:SDL_JOYBALLMOTION
	:SDL_JOYHATMOTION
	:SDL_JOYBUTTONDOWN
	:SDL_JOYBUTTONUP
	:SDL_QUIT
	:SDL_SYSWMEVENT
	:SDL_EVENT_RESERVEDA
	:SDL_EVENT_RESERVEDB
	:SDL_VIDEORESIZE
	:SDL_VIDEOEXPOSE
	:SDL_EVENT_RESERVED2
	:SDL_EVENT_RESERVED3
	:SDL_EVENT_RESERVED4
	:SDL_EVENT_RESERVED5
	:SDL_EVENT_RESERVED6
	:SDL_EVENT_RESERVED7
	(:SDL_USEREVENT 24)
	(:SDL_NUMEVENTS 32))

(cl:defconstant SDL_ALLEVENTS #xFFFFFFFF)

(cffi:defcstruct SDL_ActiveEvent
	(type :unsigned-char)
	(gain :unsigned-char)
	(state :unsigned-char))

(cffi:defcstruct SDL_KeyboardEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(keysym SDL_keysym))

(cffi:defcstruct SDL_MouseMotionEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short)
	(xrel :short)
	(yrel :short))

(cffi:defcstruct SDL_MouseButtonEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char)
	(x :unsigned-short)
	(y :unsigned-short))

(cffi:defcstruct SDL_JoyAxisEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(axis :unsigned-char)
	(value :short))

(cffi:defcstruct SDL_JoyBallEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(ball :unsigned-char)
	(xrel :short)
	(yrel :short))

(cffi:defcstruct SDL_JoyHatEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(hat :unsigned-char)
	(value :unsigned-char))

(cffi:defcstruct SDL_JoyButtonEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(button :unsigned-char)
	(state :unsigned-char))

(cffi:defcstruct SDL_ResizeEvent
	(type :unsigned-char)
	(w :int)
	(h :int))

(cffi:defcstruct SDL_ExposeEvent
	(type :unsigned-char))

(cffi:defcstruct SDL_QuitEvent
	(type :unsigned-char))

(cffi:defcstruct SDL_UserEvent
	(type :unsigned-char)
	(code :int)
	(data1 :pointer)
	(data2 :pointer))

(cffi:defcstruct SDL_SysWMEvent
	(type :unsigned-char)
	(msg :pointer))

(cffi:defcunion SDL_Event
	(type :unsigned-char)
	(active SDL_ActiveEvent)
	(key SDL_KeyboardEvent)
	(motion SDL_MouseMotionEvent)
	(button SDL_MouseButtonEvent)
	(jaxis SDL_JoyAxisEvent)
	(jball SDL_JoyBallEvent)
	(jhat SDL_JoyHatEvent)
	(jbutton SDL_JoyButtonEvent)
	(resize SDL_ResizeEvent)
	(expose SDL_ExposeEvent)
	(quit SDL_QuitEvent)
	(user SDL_UserEvent)
	(syswm SDL_SysWMEvent))

(cffi:defcfun ("SDL_PumpEvents" SDL_PumpEvents) :void)

(cffi:defcenum SDL_eventaction
	:SDL_ADDEVENT
	:SDL_PEEKEVENT
	:SDL_GETEVENT)

(cffi:defcfun ("SDL_PeepEvents" SDL_PeepEvents) :int
  (events :pointer)
  (numevents :int)
  (action SDL_eventaction)
  (mask :unsigned-int))

(cffi:defcfun ("SDL_PollEvent" SDL_PollEvent) :int
  (event :pointer))

(cffi:defcfun ("SDL_WaitEvent" SDL_WaitEvent) :int
  (event :pointer))

(cffi:defcfun ("SDL_PushEvent" SDL_PushEvent) :int
  (event :pointer))

(cffi:defcfun ("SDL_SetEventFilter" SDL_SetEventFilter) :void
  (filter :pointer))

(cffi:defcfun ("SDL_GetEventFilter" SDL_GetEventFilter) :pointer)

(cl:defconstant SDL_QUERY -1)

(cl:defconstant SDL_IGNORE 0)

(cl:defconstant SDL_DISABLE 0)

(cl:defconstant SDL_ENABLE 1)

(cffi:defcfun ("SDL_EventState" SDL_EventState) :unsigned-char
  (type :unsigned-char)
  (state :int))

(cl:defconstant SDL_ALPHA_OPAQUE 255)

(cl:defconstant SDL_ALPHA_TRANSPARENT 0)

(cffi:defcstruct SDL_Rect
	(x :short)
	(y :short)
	(w :unsigned-short)
	(h :unsigned-short))

(cffi:defcstruct SDL_Color
	(r :unsigned-char)
	(g :unsigned-char)
	(b :unsigned-char)
	(unused :unsigned-char))

(cffi:defcstruct SDL_Palette
	(ncolors :int)
	(colors :pointer))

(cffi:defcstruct SDL_PixelFormat
	(palette :pointer)
	(BitsPerPixel :unsigned-char)
	(BytesPerPixel :unsigned-char)
	(Rloss :unsigned-char)
	(Gloss :unsigned-char)
	(Bloss :unsigned-char)
	(Aloss :unsigned-char)
	(Rshift :unsigned-char)
	(Gshift :unsigned-char)
	(Bshift :unsigned-char)
	(Ashift :unsigned-char)
	(Rmask :unsigned-int)
	(Gmask :unsigned-int)
	(Bmask :unsigned-int)
	(Amask :unsigned-int)
	(colorkey :unsigned-int)
	(alpha :unsigned-char))

(cffi:defcstruct SDL_Surface
	(flags :unsigned-int)
	(format :pointer)
	(w :int)
	(h :int)
	(pitch :unsigned-short)
	(pixels :pointer)
	(offset :int)
	(hwdata :pointer)
	(clip_rect SDL_Rect)
	(unused1 :unsigned-int)
	(locked :unsigned-int)
	(map :pointer)
	(format_version :unsigned-int)
	(refcount :int))

(cl:defconstant SDL_SWSURFACE #x00000000)

(cl:defconstant SDL_HWSURFACE #x00000001)

(cl:defconstant SDL_ASYNCBLIT #x00000004)

(cl:defconstant SDL_ANYFORMAT #x10000000)

(cl:defconstant SDL_HWPALETTE #x20000000)

(cl:defconstant SDL_DOUBLEBUF #x40000000)

(cl:defconstant SDL_FULLSCREEN #x80000000)

(cl:defconstant SDL_OPENGL #x00000002)

(cl:defconstant SDL_OPENGLBLIT #x0000000A)

(cl:defconstant SDL_RESIZABLE #x00000010)

(cl:defconstant SDL_NOFRAME #x00000020)

(cl:defconstant SDL_HWACCEL #x00000100)

(cl:defconstant SDL_SRCCOLORKEY #x00001000)

(cl:defconstant SDL_RLEACCELOK #x00002000)

(cl:defconstant SDL_RLEACCEL #x00004000)

(cl:defconstant SDL_SRCALPHA #x00010000)

(cl:defconstant SDL_PREALLOC #x01000000)

(cl:defconstant SDL_YV12_OVERLAY #x32315659)

(cl:defconstant SDL_IYUV_OVERLAY #x56555949)

(cl:defconstant SDL_YUY2_OVERLAY #x32595559)

(cl:defconstant SDL_UYVY_OVERLAY #x59565955)

(cl:defconstant SDL_YVYU_OVERLAY #x55595659)

(cffi:defcstruct SDL_Overlay
	(format :unsigned-int)
	(w :int)
	(h :int)
	(planes :int)
	(pitches :pointer)
	(pixels :pointer)
	(hwfuncs :pointer)
	(hwdata :pointer)
	(hw_overlay :unsigned-int)
	(UnusedBits :unsigned-int))

(cffi:defcenum SDL_GLattr
	:SDL_GL_RED_SIZE
	:SDL_GL_GREEN_SIZE
	:SDL_GL_BLUE_SIZE
	:SDL_GL_ALPHA_SIZE
	:SDL_GL_BUFFER_SIZE
	:SDL_GL_DOUBLEBUFFER
	:SDL_GL_DEPTH_SIZE
	:SDL_GL_STENCIL_SIZE
	:SDL_GL_ACCUM_RED_SIZE
	:SDL_GL_ACCUM_GREEN_SIZE
	:SDL_GL_ACCUM_BLUE_SIZE
	:SDL_GL_ACCUM_ALPHA_SIZE
	:SDL_GL_STEREO
	:SDL_GL_MULTISAMPLEBUFFERS
	:SDL_GL_MULTISAMPLESAMPLES
	:SDL_GL_ACCELERATED_VISUAL
	:SDL_GL_SWAP_CONTROL)

(cl:defconstant SDL_LOGPAL #x01)

(cl:defconstant SDL_PHYSPAL #x02)

(cffi:defcfun ("SDL_VideoInit" SDL_VideoInit) :int
  (driver_name :string)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_VideoQuit" SDL_VideoQuit) :void)

(cffi:defcfun ("SDL_VideoDriverName" SDL_VideoDriverName) :string
  (namebuf :string)
  (maxlen :int))

(cffi:defcfun ("SDL_GetVideoSurface" SDL_GetVideoSurface) :pointer)

(cffi:defcfun ("SDL_GetVideoInfo" SDL_GetVideoInfo) :pointer)

(cffi:defcfun ("SDL_VideoModeOK" SDL_VideoModeOK) :int
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_ListModes" SDL_ListModes) :pointer
  (format :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_SetVideoMode" SDL_SetVideoMode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_UpdateRects" SDL_UpdateRects) :void
  (screen :pointer)
  (numrects :int)
  (rects :pointer))

(cffi:defcfun ("SDL_UpdateRect" SDL_UpdateRect) :void
  (screen :pointer)
  (x :int)
  (y :int)
  (w :unsigned-int)
  (h :unsigned-int))

(cffi:defcfun ("SDL_Flip" SDL_Flip) :int
  (screen :pointer))

(cffi:defcfun ("SDL_SetGamma" SDL_SetGamma) :int
  (red :float)
  (green :float)
  (blue :float))

(cffi:defcfun ("SDL_SetGammaRamp" SDL_SetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("SDL_GetGammaRamp" SDL_GetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("SDL_SetColors" SDL_SetColors) :int
  (surface :pointer)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(cffi:defcfun ("SDL_SetPalette" SDL_SetPalette) :int
  (surface :pointer)
  (flags :int)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(cffi:defcfun ("SDL_MapRGB" SDL_MapRGB) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(cffi:defcfun ("SDL_MapRGBA" SDL_MapRGBA) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(cffi:defcfun ("SDL_GetRGB" SDL_GetRGB) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(cffi:defcfun ("SDL_GetRGBA" SDL_GetRGBA) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(cffi:defcfun ("SDL_CreateRGBSurface" SDL_CreateRGBSurface) :pointer
  (flags :unsigned-int)
  (width :int)
  (height :int)
  (depth :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(cffi:defcfun ("SDL_CreateRGBSurfaceFrom" SDL_CreateRGBSurfaceFrom) :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(cffi:defcfun ("SDL_FreeSurface" SDL_FreeSurface) :void
  (surface :pointer))

(cffi:defcfun ("SDL_LockSurface" SDL_LockSurface) :int
  (surface :pointer))

(cffi:defcfun ("SDL_UnlockSurface" SDL_UnlockSurface) :void
  (surface :pointer))

(cffi:defcfun ("SDL_LoadBMP_RW" SDL_LoadBMP_RW) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("SDL_SaveBMP_RW" SDL_SaveBMP_RW) :int
  (surface :pointer)
  (dst :pointer)
  (freedst :int))

(cffi:defcfun ("SDL_SetColorKey" SDL_SetColorKey) :int
  (surface :pointer)
  (flag :unsigned-int)
  (key :unsigned-int))

(cffi:defcfun ("SDL_SetAlpha" SDL_SetAlpha) :int
  (surface :pointer)
  (flag :unsigned-int)
  (alpha :unsigned-char))

(cffi:defcfun ("SDL_SetClipRect" SDL_SetClipRect) :pointer
  (surface :pointer)
  (rect :pointer))

(cffi:defcfun ("SDL_GetClipRect" SDL_GetClipRect) :void
  (surface :pointer)
  (rect :pointer))

(cffi:defcfun ("SDL_ConvertSurface" SDL_ConvertSurface) :pointer
  (src :pointer)
  (fmt :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_UpperBlit" SDL_UpperBlit) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(cffi:defcfun ("SDL_LowerBlit" SDL_LowerBlit) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(cffi:defcfun ("SDL_FillRect" SDL_FillRect) :int
  (dst :pointer)
  (dstrect :pointer)
  (color :unsigned-int))

(cffi:defcfun ("SDL_DisplayFormat" SDL_DisplayFormat) :pointer
  (surface :pointer))

(cffi:defcfun ("SDL_DisplayFormatAlpha" SDL_DisplayFormatAlpha) :pointer
  (surface :pointer))

(cffi:defcfun ("SDL_CreateYUVOverlay" SDL_CreateYUVOverlay) :pointer
  (width :int)
  (height :int)
  (format :unsigned-int)
  (display :pointer))

(cffi:defcfun ("SDL_LockYUVOverlay" SDL_LockYUVOverlay) :int
  (overlay :pointer))

(cffi:defcfun ("SDL_UnlockYUVOverlay" SDL_UnlockYUVOverlay) :void
  (overlay :pointer))

(cffi:defcfun ("SDL_DisplayYUVOverlay" SDL_DisplayYUVOverlay) :int
  (overlay :pointer)
  (dstrect :pointer))

(cffi:defcfun ("SDL_FreeYUVOverlay" SDL_FreeYUVOverlay) :void
  (overlay :pointer))

(cffi:defcfun ("SDL_GL_LoadLibrary" SDL_GL_LoadLibrary) :int
  (path :string))

(cffi:defcfun ("SDL_GL_GetProcAddress" SDL_GL_GetProcAddress) :pointer
  (proc :string))

(cffi:defcfun ("SDL_GL_SetAttribute" SDL_GL_SetAttribute) :int
  (attr SDL_GLattr)
  (value :int))

(cffi:defcfun ("SDL_GL_GetAttribute" SDL_GL_GetAttribute) :int
  (attr SDL_GLattr)
  (value :pointer))

(cffi:defcfun ("SDL_GL_SwapBuffers" SDL_GL_SwapBuffers) :void)

(cffi:defcfun ("SDL_GL_UpdateRects" SDL_GL_UpdateRects) :void
  (numrects :int)
  (rects :pointer))

(cffi:defcfun ("SDL_GL_Lock" SDL_GL_Lock) :void)

(cffi:defcfun ("SDL_GL_Unlock" SDL_GL_Unlock) :void)

(cffi:defcfun ("SDL_WM_SetCaption" SDL_WM_SetCaption) :void
  (title :string)
  (icon :string))

(cffi:defcfun ("SDL_WM_GetCaption" SDL_WM_GetCaption) :void
  (title :pointer)
  (icon :pointer))

(cffi:defcfun ("SDL_WM_SetIcon" SDL_WM_SetIcon) :void
  (icon :pointer)
  (mask :pointer))

(cffi:defcfun ("SDL_WM_IconifyWindow" SDL_WM_IconifyWindow) :int)

(cffi:defcfun ("SDL_WM_ToggleFullScreen" SDL_WM_ToggleFullScreen) :int
  (surface :pointer))

(cffi:defcenum SDL_GrabMode
	(:SDL_GRAB_QUERY -1)
	(:SDL_GRAB_OFF 0)
	(:SDL_GRAB_ON 1)
	:SDL_GRAB_FULLSCREEN)

(cffi:defcfun ("SDL_WM_GrabInput" SDL_WM_GrabInput) SDL_GrabMode
  (mode SDL_GrabMode))

(cffi:defcfun ("SDL_SoftStretch" SDL_SoftStretch) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))

(cl:defconstant SDL_MAJOR_VERSION 1)

(cl:defconstant SDL_MINOR_VERSION 2)

(cl:defconstant SDL_PATCHLEVEL 11)

(cffi:defcstruct SDL_version
	(major :unsigned-char)
	(minor :unsigned-char)
	(patch :unsigned-char))

(cffi:defcfun ("SDL_Linked_Version" SDL_Linked_Version) :pointer)

(cl:defconstant SDL_INIT_TIMER #x00000001)

(cl:defconstant SDL_INIT_AUDIO #x00000010)

(cl:defconstant SDL_INIT_VIDEO #x00000020)

(cl:defconstant SDL_INIT_CDROM #x00000100)

(cl:defconstant SDL_INIT_JOYSTICK #x00000200)

(cl:defconstant SDL_INIT_NOPARACHUTE #x00100000)

(cl:defconstant SDL_INIT_EVENTTHREAD #x01000000)

(cl:defconstant SDL_INIT_EVERYTHING #x0000FFFF)

(cffi:defcfun ("SDL_Init" SDL_Init) :int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_InitSubSystem" SDL_InitSubSystem) :int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_QuitSubSystem" SDL_QuitSubSystem) :void
  (flags :unsigned-int))

(cffi:defcfun ("SDL_WasInit" SDL_WasInit) :unsigned-int
  (flags :unsigned-int))

(cffi:defcfun ("SDL_Quit" SDL_Quit) :void)

