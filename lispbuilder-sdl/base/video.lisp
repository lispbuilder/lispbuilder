;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)



(defmacro with-display ((width height &key (flags SDL_SWSURFACE) (bpp 0)
			       (title-caption nil) (icon-caption nil)) &body body)
  (let ((body-value (gensym "body-value-"))
	(surface-name (gensym "surface-name-")))
    `(let ((,body-value nil)
	   (,surface-name (set-window ,width ,height :bpp ,bpp :flags ,flags
				      :title-caption ,title-caption :icon-caption ,icon-caption)))
       (if (is-valid-ptr ,surface-name)
	   (setf ,body-value (progn ,@body)))
       ,body-value)))


(defun display-cursor (toggle)
  (if toggle
      (SDL_ShowCursor sdl_enable)
      (SDL_ShowCursor sdl_disable)))

(defun get-native-window ()
  (let ((wm-info (cffi:foreign-alloc 'sdl::SDL_SysWMinfo)))
      ;; Set the wm-info structure to the current SDL version.
      (sdl::sdl_version (cffi:foreign-slot-value wm-info 'sdl::SDL_SysWMinfo 'sdl::version))
      (sdl::SDL_GetWMInfo wm-info)
      ;; For Windows
      #+win32(cffi:foreign-slot-pointer wm-info 'sdl::SDL_SysWMinfo 'sdl::window)
      ;; For X
      #-win32(cffi:foreign-slot-pointer (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer wm-info
											      'SDL_SysWMinfo
											      'sdl::info)
								   'sdl::SDL_SysWMinfo_info
								   'sdl::x11)
					'sdl::SDL_SysWMinfo_info_x11
					'sdl::window)))

(defun get-video-info (&key (video-info (SDL_GetVideoInfo)) (info :video-mem))
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
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'current_w))
	(:current-h
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'current_h))
	(:video-mem
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'video_mem))
	(:pixel-format
	 (cffi:foreign-slot-value video-info 'sdl_videoinfo 'vfmt))
	(otherwise
	 (member info (cffi:foreign-slot-value video-info 'sdl_videoinfo 'flags))))
      nil))


(defun list-modes (flags)
  "Returns a LIST of rects  for each available screen dimension 
   for the given format and video flags, sorted largest to smallest. 
   Returns NIL if there are no dimensions available for a particular format, 
   or T if any dimension is okay for the given format."
  (let ((modes nil)
        (listmodes (sdl::SDL_ListModes (cffi:null-pointer) (set-flags flags))))
    (cond
      ((cffi:null-pointer-p listmodes)
       nil)
      ((equal (cffi:pointer-address listmodes) 4294967295)
       t)
      (t
       (do ((i 0 (1+ i)))
	   ((cffi:null-pointer-p (cffi:mem-ref (cffi:mem-aref listmodes 'sdl:sdl_rect i) :pointer)) (reverse modes))
	 (let ((rect (cffi:mem-ref (cffi:mem-aref listmodes 'sdl:sdl_rect i) :pointer)))
	   (setf modes (cons (vector (cffi:foreign-slot-value rect 'sdl:sdl_rect 'sdl:w)
				     (cffi:foreign-slot-value rect 'sdl:sdl_rect 'sdl:h))
			     modes))))))))


(defun query-cursor ()
  (case (SDL_ShowCursor sdl_query)
    (sdl_disable nil)
    (sdl_enable t)))


(defun set-screen (width height
		   &key (bpp 0) (flags '(SDL_HWSURFACE SDL_FULLSCREEN SDL_HWACCEL)) title-caption icon-caption)
  "Will attempt to create a full screen, hardware accelerated window using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (let ((surface (SDL_SetVideoMode width height bpp (set-flags flags))))
    (if (is-valid-ptr surface)
	(progn
	  (if (or title-caption icon-caption) 
	      (WM-Set-Caption title-caption icon-caption))
	  surface)
	nil)))

(defun set-window (width height &key (bpp 0) (flags SDL_SWSURFACE) title-caption icon-caption)
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (set-screen width height :bpp bpp :flags flags :title-caption title-caption :icon-caption icon-caption))

(defun update-display (surface)
  (sdl_flip surface))

;; cl-sdl "cl-sdl.lisp"
(defun clear-display (surface color)
  (fill-surface surface color :clipping-p nil)
  surface)

(defun video-driver-name ()
  (let ((function-return-val nil)
	(string-return-val nil))
    (setf string-return-val (with-foreign-pointer-as-string (str 100 str-size)
			      (setf function-return-val (videodrivername str str-size))))
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
