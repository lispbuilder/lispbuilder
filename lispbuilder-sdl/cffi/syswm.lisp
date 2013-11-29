
(in-package #:lispbuilder-sdl-cffi)

(cffi:defcstruct HWND__
	(unused :int))

(cffi:defcstruct HGLRC__
	(unused :int))

(cffi:defcstruct HDC__
	(unused :int))

#+windows(defcstruct SDL-Sys-WM-msg
           (version (:struct SDL-version))
           (hwnd :pointer)
           (msg :pointer)
           (wParam :unsigned-int)
           (lParam :long))

#+windows(defcstruct SDL-Sys-WM-info
           (version (:struct SDL-version))
           (window :pointer)
           (hglrc :pointer))

#-windows(defcenum SDL-SYS-WM-TYPE
           :SDL-SYS-WM-X11)

#-windows(defcunion SDL-Sys-WM-msg-event
           (xevent :pointer))

#-windows(defcstruct SDL-Sys-WM-msg
           (version (:struct SDL-version))
           (subsystem SDL-SYS-WM-TYPE)
           (event (:union SDL-Sys-WM-msg-event)))

#-windows(defcstruct SDL-Sys-WM-info-info-x11
           (display :pointer)
           (window :unsigned-long)
           (lock-func :pointer)
           (unlock-func :pointer)
           (fswindow :unsigned-long)
           (wmwindow :unsigned-long)
           (gfxdisplay :pointer))

#-windows(defcunion SDL-Sys-WM-info-info
           (x11 (:struct SDL-Sys-WM-info-info-x11)))

#-windows(defcstruct SDL-Sys-WM-info
           (version (:struct SDL-version))
           (subsystem SDL-SYS-WM-TYPE)
           (info (:union SDL-Sys-WM-info-info)))

(defcfun ("SDL_GetWMInfo" SDL-Get-WM-Info) :int
  (info :pointer))
