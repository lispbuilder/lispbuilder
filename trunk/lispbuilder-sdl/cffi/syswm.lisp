
(in-package #:lispbuilder-sdl-cffi) 

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
	(wmwindow :unsigned-long)
	(gfxdisplay :pointer))

#-win32 (defcunion SDL-Sys-WM-info-info
	(x11 SDL-Sys-WM-info-info-x11))

#-win32 (defcstruct SDL-Sys-WM-info
	(version SDL-version)
	(subsystem SDL-SYS-WM-TYPE)
	(info SDL-Sys-WM-info-info))

(defcfun ("SDL_GetWMInfo" SDL-Get-WM-Info) :int
  (info :pointer))

