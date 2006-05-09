;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl) 

;;;; Overrides to C header files follow:
;;;;
;;;; "SDL_events.h"

;;;; Have to manually define SDL_KeyboardEvent as SWIG does not differentiate between an aggregate structute
;;;; and a pointer to a struct.
(defcstruct SDL_KeyboardEvent
	(type :unsigned-char)
	(which :unsigned-char)
	(state :unsigned-char)
	(keysym sdl_keysym))

;;;; the SDL_Event is a union which is not yet correctly converted by SWIG
(cffi:defcunion SDL_Event
  (type :unsigned-char)
  (active-event SDL_ActiveEvent)
  (keyboard-event SDL_KeyboardEvent)
  (mouse-motion-event SDL_MouseMotionEvent)
  (mouse-button-event SDL_MouseButtonEvent)
  (joy-axis-event SDL_JoyAxisEvent)
  (joy-ball-event SDL_JoyBallEvent)
  (joy-hat-event SDL_JoyHatEvent)
  (joy-button-event SDL_JoyButtonEvent)
  (resize-event SDL_ResizeEvent)
  (expose-event SDL_ExposeEvent)
  (quit-event SDL_QuitEvent)
  (user-event SDL_UserEvent)
  (sys-wm-event SDL_SysWMEvent))
;;;; "SDL_events.h"

;;;; "SDL_audio.h"
;;;; Must define the CPU byte order.
#-(or little-endian PC386 X86 I386) (defconstant AUDIO_U16SYS AUDIO_U16MSB) ;; Big Endian
#-(or little-endian PC386 X86 I386) (defconstant AUDIO_S16SYS AUDIO_S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO_U16SYS AUDIO_U16LSB) ;; Little Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO_S16SYS AUDIO_S16LSB) ;; Little Endian
;;;; end "SDL_audio.h"

;;;; "SDL_joystick.h"
(defconstant SDL_HAT_RIGHTUP   (logior SDL_HAT_RIGHT SDL_HAT_UP))
(defconstant SDL_HAT_RIGHTDOWN (logior SDL_HAT_RIGHT SDL_HAT_DOWN))
(defconstant SDL_HAT_LEFTUP    (logior SDL_HAT_LEFT SDL_HAT_UP))
(defconstant SDL_HAT_LEFTDOWN  (logior SDL_HAT_LEFT SDL_HAT_DOWN))
;;;; end "SDL_joystick.h"

;;;; "SDL_keysym.h"
(defconstant KMOD_CTRL	(logior  (cffi:foreign-enum-value 'SDLMod :KMOD_LCTRL)
				 (cffi:foreign-enum-value 'SDLMod :KMOD_RCTRL)))
(defconstant KMOD_SHIFT	(logior  (cffi:foreign-enum-value 'SDLMod :KMOD_LSHIFT)
				 (cffi:foreign-enum-value 'SDLMod :KMOD_RSHIFT)))
(defconstant KMOD_ALT	(logior  (cffi:foreign-enum-value 'SDLMod :KMOD_LALT)
				 (cffi:foreign-enum-value 'SDLMod :KMOD_RALT)))
(defconstant KMOD_META	(logior  (cffi:foreign-enum-value 'SDLMod :KMOD_LMETA)
				 (cffi:foreign-enum-value 'SDLMod :KMOD_RMETA)))
;;;; end "SDL_keysym.h"

;;;; "SDL_syswm.h"
#+win32 (defcstruct SDL_SysWMmsg
;; 	(version :pointer)
	(version SDL_version)
	(hwnd :pointer)
	(msg :pointer)
	(wParam :unsigned-int)
	(lParam :long))

#+win32 (defcstruct SDL_SysWMinfo
	(version SDL_version)
	(window :pointer)
	(hglrc :pointer))

#-win32 (defcenum SDL_SYSWM_TYPE
	:SDL_SYSWM_X11)

#-win32 (defcunion SDL_SysWMmsg_event
	(xevent :pointer))

#-win32 (defcstruct SDL_SysWMmsg
	(version SDL_version)
	(subsystem SDL_SYSWM_TYPE)
	(event SDL_SysWMmsg_event))

#-win32 (defcstruct SDL_SysWMinfo_info_x11
	(display :pointer)
	(window :unsigned-long)
	(lock_func :pointer)
	(unlock_func :pointer)
	(fswindow :unsigned-long)
	(wmwindow :unsigned-long))

#-win32 (defcunion SDL_SysWMinfo_info
	(x11 SDL_SysWMinfo_info_x11))

#-win32 (defcstruct SDL_SysWMinfo
	(version SDL_version)
	(subsystem SDL_SYSWM_TYPE)
	(info SDL_SysWMinfo_info))

(defcfun ("SDL_GetWMInfo" SDL_GetWMInfo) :int
  (info :pointer))
;;;; end "SDL_syswm.h"


;;;; Implementation of SDL macros follows
;;;; "SDL_version.h"
(defun SDL_VERSION (x)
  (cffi:with-foreign-slots ((major minor patch) x SDL_version)
    (setf major SDL_MAJOR_VERSION
          minor SDL_MINOR_VERSION
          patch SDL_PATCHLEVEL)))

(defun SDL_VERSIONNUM (major minor patch)
        (+  (* major 1000)
            (* minor 100)
            patch))

(defun SDL_COMPILEDVERSION ()
        (SDL_VERSIONNUM SDL_MAJOR_VERSION SDL_MINOR_VERSION SDL_PATCHLEVEL))

(defun SDL_VERSION_ATLEAST (x y z)
  (if (>= (SDL_COMPILEDVERSION) (SDL_VERSIONNUM x y z))
      1
      0))
;;;; end "SDL_version.h"

;;;; "SDL_mutex.h"
(defun SDL_LockMutex (m)
  (SDL_mutexP m))

(defun SDL_UnlockMutex (m)
  (SDL_mutexV m))
;;;; end "SDL_mutex.h"

;;;; "SDL_video.h"
(defun SDL_MUSTLOCK (surface)
  (if (> 0 (cffi:foreign-slot-value surface 'SDL_Surface 'offset))
      (values 1)
    (if (not (eql 0 (logand (cffi:foreign-slot-value surface 'SDL_Surface 'flags)
                            (logior SDL_HWSURFACE SDL_ASYNCBLIT SDL_RLEACCEL))))
        (values 1)
      (values 0))))

(defun SDL_LoadBMP (file)
  (SDL_LoadBMP_RW (SDL_RWFROMFILE file "rb") 1))

;; (defun SDL_AllocSurface ()
;;   (SDL_CreateRGBSurface))

(defun SDL_SaveBMP (surface file)
  (SDL_SaveBMP_RW surface (SDL_RWFROMFILE file "wb") 1))

(defun SDL_BlitSurface (src srcrect dst dstrect)
  (SDL_UpperBlit src srcrect dst dstrect))
;;;; end "SDL_video.h"

;;;; "SDL_cdrom.h"
(defun CD_INDRIVE (status)
  (if (> status 0)
      t
    nil))

(defconstant CD_FPS 75)
(defun FRAMES_TO_MSF (f)
  (values 
   (mod f CD_FPS)
   (mod (/ f CD_FPS) 60)
   (/ (/ f CD_FPS) 60)))

(defun MSF_TO_FRAMES (M S F)
  (+ 
   (* M 60 CD_FPS)
   (* S CD_FPS)
   F))
;;;; end "SDL_cdrom.h"

;;;; "SDL_audio.h"
(defun SDL_LoadWAV (file spec audio-buf audio-len)
  (SDL_LoadWAV_RW (SDL_RWFROMFILE file "rb")
		  1
		  spec
		  audio-buf
		  audio-len))
;;;; end "SDL_audio.h"

;;;; "SDL_error.h"
(defun SDL_OutOfMemory ()
  (SDL_Error (cffi:foreign-enum-value 'SDL_errorcode :SDL_ENOMEM)))
;;;; end "SDL_error.h"

;;;; "SDL_mouse.h"
(defun SDL_BUTTON (X)
  (ash SDL_PRESSED (- X 1)))

(defun SDL_BUTTON_LMASK ()
  (SDL_BUTTON SDL_BUTTON_LEFT))

(defun SDL_BUTTON_MMASK ()
  (SDL_BUTTON SDL_BUTTON_MIDDLE))

(defun SDL_BUTTON_RMASK ()
  (SDL_BUTTON SDL_BUTTON_RIGHT))
;;;; end "SDL_mouse.h"

;;;; "SDL_events.h"
(defun SDL_EVENTMASK (X)
  (ash 1 X ))

(defun SDL_ACTIVEEVENTMASK ()
  (SDL_EVENTMASK SDL_ACTIVEEVENT))

(defun SDL_KEYDOWNMASK ()
  (SDL_EVENTMASK SDL_KEYDOWN))

(defun SDL_KEYUPMASK ()
  (SDL_EVENTMASK SDL_KEYUP))

(defun SDL_MOUSEMOTIONMASK ()
  (SDL_EVENTMASK SDL_MOUSEMOTION))

(defun SDL_MOUSEBUTTONDOWNMASK ()
  (SDL_EVENTMASK SDL_MOUSEBUTTONDOWN))

(defun SDL_MOUSEBUTTONUPMASK ()
  (SDL_EVENTMASK SDL_MOUSEBUTTONUP))

(defun SDL_MOUSEEVENTMASK ()
  (logior (SDL_EVENTMASK SDL_MOUSEMOTION)
          (SDL_EVENTMASK SDL_MOUSEBUTTONDOWN)
          (SDL_EVENTMASK SDL_MOUSEBUTTONUP)))

(defun SDL_JOYAXISMOTIONMASK ()
  (SDL_EVENTMASK SDL_JOYAXISMOTION))

(defun SDL_JOYBALLMOTIONMASK ()
  (SDL_EVENTMASK SDL_JOYBALLMOTION))

(defun SDL_JOYHATMOTIONMASK ()
  (SDL_EVENTMASK SDL_JOYHATMOTION))

(defun SDL_JOYBUTTONDOWNMASK ()
  (SDL_EVENTMASK SDL_JOYBUTTONDOWN))

(defun SDL_JOYBUTTONUPMASK ()
  (SDL_EVENTMASK SDL_JOYBUTTONUP))

(defun SDL_JOYEVENTMASK ()
  (logior (SDL_EVENTMASK SDL_JOYAXISMOTION)
          (SDL_EVENTMASK SDL_JOYBALLMOTION)
          (SDL_EVENTMASK SDL_JOYHATMOTION)
          (SDL_EVENTMASK SDL_JOYBUTTONDOWN)
          (SDL_EVENTMASK SDL_JOYBUTTONUP)))

(defun SDL_VIDEORESIZEMASK () 
  (SDL_EVENTMASK SDL_VIDEORESIZE))

(defun SDL_VIDEOEXPOSEMASK ()
  (SDL_EVENTMASK SDL_VIDEOEXPOSE))

(defun SDL_QUITMASK ()
  (SDL_EVENTMASK SDL_QUIT))

(defun SDL_SYSWMEVENTMASK ()
  (SDL_EVENTMASK SDL_SYSWMEVENT))
;;;; end "SDL_events.h"

;;;; end Lisp Macros

;;;;
;;;; end Overrides
