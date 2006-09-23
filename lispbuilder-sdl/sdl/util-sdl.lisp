;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Macros

;;; c
;;; g

;;; w
(defmacro with-init (init-flags &body body)
  "Attempts to initialize the SDL subsystems using SDL_Init.
   Automatically shuts down the SDL subsystems using SDL_Quit upon normal application termination or
   if any fatal error occurs within &body.
   init-flags can be any combination of SDL_INIT_TIMER, SDL_INIT_AUDIO, SDL_INIT_VIDEO, SDL_INIT_CDROM,
   SDL_INIT_JOYSTICK, SDL_INIT_NOPARACHUTE, SDL_INIT_EVENTTHREAD or SDL_INIT_EVERYTHING."
  `(block nil
    (unwind-protect
	 (when (init-sdl :flags (list ,@init-flags))
	   ,@body)
      (SDL_Quit))))

(defmacro with-display ((width height &key (flags SDL_SWSURFACE) (bpp 0)
			       (title-caption nil) (icon-caption nil)
			       (surface-name '*default-display*)) &body body)
  (let ((body-value (gensym "body-value")))
    `(let ((,body-value nil)
	     (,surface-name (set-window ,width ,height :bpp ,bpp :flags ,flags
					:title-caption ,title-caption :icon-caption ,icon-caption)))
	 (setf *default-surface* ,surface-name)
	 (setf *default-display* ,surface-name)
	 (if (is-valid-ptr ,surface-name)
	     (setf ,body-value (progn ,@body)))
	 ,body-value)))

;;;; Functions

;;; a
;;; c

(defun create-RWops-from-file (filename path)
  (let ((file-path (namestring (merge-pathnames filename path))))
    (if (and (stringp file-path) (probe-file file-path))
	(sdl:RWFromFile file-path "rb")
	nil)))

;;; d

(defun display-cursor (toggle)
  (if toggle
      (SDL_ShowCursor sdl_enable)
      (SDL_ShowCursor sdl_disable)))

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- x1 x2) 2) 
	   (expt (- y1 y2) 2))))

;;; f
;;; g


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


;;; h
;;; i

(defun init-sdl (&key (flags SDL_INIT_VIDEO))
  (if (equal 0 (SDL_Init (set-flags flags)))
      t
      nil))

;;; j
;;; k
;;; l

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

;;; m
;;; n
;;; o
;;; p

(defun points-in-range (p1 p2 distance)
  "return true, if the distance between p1 and p2 is not more than 'distance'"
  (<= (+ (expt (- (sdl:point-x p1) (sdl:point-x p2)) 2)
         (expt (- (sdl:point-y p1) (sdl:point-y p2)) 2))
      (expt distance 2)))


;;; q

(defun query-cursor ()
  (case (SDL_ShowCursor sdl_query)
    (sdl_disable nil)
    (sdl_enable t)))


;;; r

(defun random-rect (bound-w bound-h)
  (let* ((x (random bound-w))
	 (y (random bound-h))
	 (w (random+1 (- bound-w x)))
	 (h (random+1 (- bound-h y))))
    (rectangle x y w h)))

(defun random-color (&optional alpha)
  (if alpha ;; alpha is either t, or a number then create r/g/b/a
      (color (random 255) (random 255) (random 255) (if (numberp alpha)
							alpha
							(random 255)))
      (color (random 255) (random 255) (random 255)))) ; Or not, and create an r/g/b color

(defun random-point (max-x max-y)
  (sdl:point (random max-x) (random max-y)))

;;; s

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
	      (WM_SetCaption title-caption icon-caption))
	  surface)
	nil)))

(defun set-window (width height &key (bpp 0) (flags SDL_SWSURFACE) title-caption icon-caption)
  "Will attempt to create a window using software surfaces using SDL_SetVideoMode.
   Overriding :flags will allow any type of window to be created.
   Returns
    a new SDL_Surface if successful.
    NIL if failed."
  (set-screen width height :bpp bpp :flags flags :title-caption title-caption :icon-caption icon-caption))

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


;;; t

;;; u

(defun update-display (&optional (surface *default-display*))
  (sdl_flip surface))

;; cl-sdl "cl-sdl.lisp"
(defun clear-display (&key (color *default-color*) (surface *default-display*))
  (fill-surface :surface surface :color color :clipping-p nil)
  surface)


;;; v

(defun video-driver-name ()
  (let ((function-return-val nil)
	(string-return-val nil))
    (setf string-return-val (with-foreign-pointer-as-string (str 100 str-size)
			      (setf function-return-val (videodrivername str str-size))))
    (if (cffi:null-pointer-p function-return-val)
	nil
	string-return-val)))

;;; w

(defun warp-mouse (&optional (position sdl:*default-position*))
  (sdl_warpmouse (point-x position) (point-y position)))

;;; x
;;; y
;;; z

