;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)



(defmacro with-display ((surface-name width height
				      &key (flags sdl-cffi::SDL-SW-SURFACE) (bpp 0)
				      (title-caption nil) (icon-caption nil))
			&body body)
  `(with-surface (,surface-name (set-window ,width ,height
					    :bpp ,bpp :flags ,flags
					    :title-caption ,title-caption :icon-caption ,icon-caption)
				nil)
     (if (is-valid-ptr ,surface-name)
	 (progn ,@body))))

(defun display-cursor (toggle)
  (if toggle
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-enable)
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-disable)))

(defun get-native-window ()
  (let ((wm-info (cffi:foreign-alloc 'sdl-cffi::SDL-Sys-WM-info)))
      ;; Set the wm-info structure to the current SDL version.
      (sdl-cffi::sdl-version (cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::version))
      (sdl-cffi::SDL-Get-WM-Info wm-info)
      ;; For Windows
      #+win32(cffi:foreign-slot-pointer wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::window)
      ;; For X
      #-win32(cffi:foreign-slot-pointer (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer wm-info
											      'sdl-cffi::SDL-Sys-WM-info
											      'sdl-cffi::info)
								   'sdl-cffi::SDL-Sys-WM-info-info
								   'sdl-cffi::x11)
					'sdl-cffi::SDL-Sys-WM-info-info-x11
					'sdl-cffi::window)))

(defun get-video-info (&key (video-info (sdl-cffi::SDL-Get-Video-Info)) (info :video-mem))
  "Returns information about the video hardware.
  GET-VIDEO-INFO :video-info <pointer to a SDL_VIDEOINFO structure>
                 :info (one of :hw-available | :wm-available |
                       :blit-hw | :blit-hw-cc | :blit-hw-a |
                       :blit-sw | :blit-sw-cc | :blit-sw-a |
                       :blit-fill |
                       :video-mem |
                       :pixel-format |
                       :current-w | 
                       :current-h)
  Usage: get-video-info should be called after sdl_init but before sdl_setvideomode.
         e.g (get-video-info :info :video_mem ), or
             (get-video-info :video-info (sdl_getvideoinfo) :info :video_mem)
         Will return the amount video memory available."
  (if (is-valid-ptr video-info)
      (case info
	(:current-w
	 (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-w))
	(:current-h
	 (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-h))
	(:video-mem
	 (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::video-mem))
	(:pixel-format
	 (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::vfmt))
	(otherwise
	 (member info (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::flags))))
      nil))


(defun list-modes (flags)
  "Returns a LIST of rects  for each available screen dimension 
   for the given format and video flags, sorted largest to smallest. 
   Returns NIL if there are no dimensions available for a particular format, 
   or T if any dimension is okay for the given format."
  (let ((modes nil)
        (listmodes (sdl-cffi::SDL-List-Modes (cffi:null-pointer) (set-flags flags))))
    (cond
      ((cffi:null-pointer-p listmodes)
       nil)
      ((equal (cffi:pointer-address listmodes) 4294967295)
       t)
      (t
       (do ((i 0 (1+ i)))
	   ((cffi:null-pointer-p (cffi:mem-ref (cffi:mem-aref listmodes 'sdl-cffi::sdl-rect i) :pointer)) (reverse modes))
	 (let ((rect (cffi:mem-ref (cffi:mem-aref listmodes 'sdl-cffi::sdl-rect i) :pointer)))
	   (setf modes (cons (vector (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w)
				     (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h))
			     modes))))))))


(defun query-cursor ()
  (case (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-query)
    (sdl-cffi::sdl-disable nil)
    (sdl-cffi::sdl-enable t)))


(defun set-screen (width height
		   &key (bpp 0) (flags '(sdl-cffi::SDL-HW-SURFACE sdl-cffi::SDL-FULL-SCREEN sdl-cffi::SDL-HW-ACCEL)) title-caption icon-caption)
  "Will attempt to create a full screen, hardware accelerated window using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (let ((surface (sdl-cffi::SDL-Set-Video-Mode width height bpp (set-flags flags))))
    (if (is-valid-ptr surface)
	(progn
	  (if (or title-caption icon-caption) 
	      (sdl-cffi::sdl-WM-Set-Caption title-caption icon-caption))
	  surface)
	nil)))

(defun set-window (width height &key (bpp 0) (flags sdl-cffi::SDL-SW-SURFACE) title-caption icon-caption)
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (set-screen width height :bpp bpp :flags flags :title-caption title-caption :icon-caption icon-caption))

(defun update-display (surface)
  (sdl-cffi::sdl-flip surface))

;; cl-sdl "cl-sdl.lisp"
(defun clear-display (surface color)
  (fill-surface surface color :clipping-p nil)
  surface)

(defun video-driver-name ()
  (let ((function-return-val nil)
	(string-return-val nil))
    (setf string-return-val (with-foreign-pointer-as-string (str 100 str-size)
			      (setf function-return-val (sdl-cffi::sdl-video-driver-name str str-size))))
    (if (cffi:null-pointer-p function-return-val)
	nil
	string-return-val)))

;; cl-sdl "sdl-ext.lisp"
;; (defun show-bmp (file surface x y)
;;   (let ((bmp nil))
;;     (unwind-protect
;;          (progn
;;            (setf bmp (sdl:load-bmp file))
;;            (when bmp
;;              (blit-surface bmp surface :dst-rect (vector x y))
;; 	     (update-surface surface :template (vector x y (surf-w bmp) (surf-h bmp)))
;; 	     ))
;;       (when (is-valid-ptr bmp)
;; 	(cffi:foreign-free bmp)))))
