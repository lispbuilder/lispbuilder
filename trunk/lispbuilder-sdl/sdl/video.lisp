;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Functions

(defun any-format-p ()
  "Returns `T` if any pixel format is allowed."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-any-format))

(defun opengl-context-p ()
  "Returns `T` if the video surface has an OpenGL context, or returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-opengl))

(defun double-buffered-p ()
  "Returns `T` if the video surface is double buffered, or returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-doublebuf))

(defun fullscreen-p ()
  "Returns `T` if the video surface is fullscreen. Returns `NIL` if the video surface is windowed."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-fullscreen))

(defun resizable-p ()
  "Returns `T` if the video surface is resizable, returns `NIL` otherwise."
  (get-surface-attribute (sdl-cffi::sdl-get-video-surface) sdl-resizable))

(defun set-window-position (position)
  (let ((window-position (if (symbolp position)
                           "center"
                           (format nil "~A,~A" (elt position 0) (elt position 1)))))
    (if position
      (sdl-cffi::sdl-put-env (format nil "SDL_VIDEO_WINDOW_POS=~A" window-position))
      (sdl-cffi::sdl-put-env (format nil "SDL_VIDEO_WINDOW_POS=")))))

(defun set-video-driver (driver)
  (when driver
    (sdl-cffi::sdl-put-env (format nil "SDL_VIDEODRIVER=~A" driver))))

(defun set-audio-driver (driver)
  (when driver
    (sdl-cffi::sdl-put-env (format nil "SDL_AUDIODRIVER=~A" driver))))

(defclass display (display-surface) ()
  (:default-initargs
   :fps (make-instance 'sdl:fps-fixed)
   :event-filter (cffi:callback event-filter)))

(defmethod initialize-instance :before ((self display) &key position video-driver audio-driver)
  ;; Set the x/y window position
  (set-window-position position)
  ;; Set the video driver
  (set-video-driver video-driver)
  ;; Set the audio driver
  (set-audio-driver audio-driver)
  ;; Initialize the video subsytem, if not already initialized.
  (unless (init-video)
    (error "ERROR Cannot initialize the video subsystem. Cannot create the display surface~%")))

(defmethod initialize-instance :after ((self display) &key
                                       (width 0) (height 0) (bpp 0) (title-caption "") (icon-caption "")
                                       flags sw hw fullscreen async-blit any-format palette double-buffer opengl resizable no-frame
                                       fps opengl-attributes
                                       event-filter)
  
  (unless flags
    (setf flags (remove nil (list (when sw sdl-sw-surface)
                                  (when hw sdl-hw-surface)
                                  (when fullscreen sdl-fullscreen)
                                  (when async-blit sdl-async-blit)
                                  (when any-format sdl-any-format)
                                  (when palette sdl-hw-palette)
                                  (when double-buffer sdl-doublebuf)
                                  (when opengl sdl-opengl)
                                  (when resizable sdl-resizable)
                                  (when no-frame sdl-no-frame)))))

  (when opengl-attributes
    (unless (consp (first opengl-attributes))
      (setf opengl-attributes (list opengl-attributes)))
    (loop for (attribute val) in opengl-attributes
       do (progn
	    (unless (integerp val)
	      (error "ERROR: WINDOW :OPENGL-ATTRIBUTES not valid for ~A and ~A" attribute val))
	    (sdl:set-gl-attribute attribute val))))
   
  ;; Make sure the display surface is created
  (let ((surface (sdl-cffi::SDL-Set-Video-Mode (cast-to-int width) (cast-to-int height)
                                               bpp (sdl-base::set-flags flags))))
    (if (is-valid-ptr surface)
      (setf (slot-value self 'foreign-pointer-to-object) surface
            *default-display* self)
      (setf *default-display* nil)))

  (setf (slot-value self 'position-rect) (rectangle))

  ;; Set the cells for the display
  (setf (cells self) 1)
  (setf (cell-index self) 0)

  ;; And set the captions
  (set-caption title-caption icon-caption)

  ;; Set the frame rate manager
  (setf *default-fpsmanager* fps)

  ;; Prime the input handling code
  (quit-input-util)
  (initialise-input-util)
  (if event-filter
    (sdl-cffi::sdl-set-event-filter event-filter)
    (sdl-cffi::sdl-set-event-filter (cffi:null-pointer))))

(defmethod window (width height &rest rest)
  (let ((window (apply #'make-instance 'display (append rest (list :width width :height height)))))
    (when (fp window)
      window)))

(defmethod resize-window (width height &key
                                flags sw hw fullscreen async-blit any-format palette double-buffer opengl resizable no-frame
                                title-caption icon-caption bpp opengl-attributes)
  "Modifies the dispaly, resets the input loop and clears all key events."
  (multiple-value-bind (title icon)
      (sdl:get-caption)
    (unless flags
      (setf flags (remove nil (list (when sw sdl-sw-surface)
                                    (when hw sdl-hw-surface)
                                    (when fullscreen sdl-fullscreen)
                                    (when async-blit sdl-async-blit)
                                    (when any-format sdl-any-format)
                                    (when palette sdl-hw-palette)
                                    (when double-buffer sdl-doublebuf)
                                    (when opengl sdl-opengl)
                                    (when resizable sdl-resizable)
                                    (when no-frame sdl-no-frame)))))
    (sdl:window width height
                :title-caption (if title-caption title-caption title)
                :icon-caption (if icon-caption icon-caption icon)
                :bpp (if bpp bpp (sdl:bit-depth sdl:*default-display*))
                :flags (if flags flags (sdl:surface-info sdl:*default-display*))
		:opengl-attributes opengl-attributes
                :fps *default-fpsmanager*)))

(defun update-display (&optional (surface *default-display*))
  "`UPDATE-DISPLAY` will flip the SDL video buffers and update 
the screen `SURFACE` if `SDL-HW-SURFACE` is set in [WINDOW](#window). If double buffering is not enabled then
 SDL will perform an [SDL-UPDATE-RECT](#sdl-update-rect) on the entire screen.

If there is an active OpenGL contenxt, then `UPDATE-DISPLAY` will call 
[SDL-GL-SWAP-BUFFERS](#sdl-gl-swap-buffers) to update the OpenGL display display.

`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified."
  (if (opengl-context-p)
      (sdl-cffi::sdl-gl-swap-buffers)
      (if (double-buffered-p)
        (sdl-cffi::sdl-flip (fp surface))
        (sdl-cffi::sdl-update-rect (fp surface) 0 0 0 0))))

(defun clear-display (color &key (surface *default-display*) update)
  "Fills the display `SURFACE` using color `COLOR`.
`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified. 
The display is updated when `UPDATE` is `T`."
  (let ((fp (fp surface)))
    (sdl-cffi::sdl-Fill-Rect fp (cffi:null-pointer) (map-color color surface))
    (if update
      (sdl-cffi::SDL-Update-Rect fp 0 0 0 0))))

(defun show-cursor (state)
  "Disables the cursor when state is `NIL`, otherwise enables the cursor."
  (if state
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-enable)
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-disable)))

(defun get-native-window ()
  "Returns a foreign pointer to the native SDL display window."
  (let ((wm-info (cffi:foreign-alloc 'sdl-cffi::SDL-Sys-WM-info)))
    ;; Set the wm-info structure to the current SDL version.
    (sdl-cffi::set-sdl-version (cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::version))
    (sdl-cffi::SDL-Get-WM-Info wm-info)
    ;; For Windows
    #+windows(cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::window)
    ;; For X
    #-windows(cffi:foreign-slot-pointer (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer wm-info
                                                                                            'sdl-cffi::SDL-Sys-WM-info
                                                                                            'sdl-cffi::info)
                                                                 'sdl-cffi::SDL-Sys-WM-info-info
                                                                 'sdl-cffi::x11)
                                      'sdl-cffi::SDL-Sys-WM-info-info-x11
                                      'sdl-cffi::window)))



(defun video-memory ()
  "Returns the amount of video memory of the graphics hardware. Must be called after SDL is initialized 
using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)."
  (let ((video-info (sdl-cffi::SDL-Get-Video-Info)))
    (unless (cffi:null-pointer-p video-info)
      (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::video-mem))))

(defun video-dimensions ()
  "Returns the best video dimensions if called before a window is created, using [WINDOW](#window). 
Returns the current video dimensions if called after a window is created.
Must be called after SDL is initialized using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)"
  (let ((video-info (sdl-cffi::SDL-Get-Video-Info)))
    (unless (cffi:null-pointer-p video-info)
      (vector (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-w)
	  (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::current-h)))))

(defun video-info (&rest info)
  "Returns information about the video hardware. 
`VIDEO-INFO` must be called after SDL is initialised using [INIT-SDL](#init-sdl) or 
[WITH-INIT](#with-init).
If `VIDEO-INFO` is called before [WINDOW](#window), the information returned is of 
the *best* video mode. If `VIDEO-INFO` is called after [WINDOW](#window), the information 
returned is of the *current* video mode. 

##### Parameters

* `INFO` can be one of `:HW-AVAILABLE`, `:WM-AVAILABLE`, `:BLIT-HW`, `:BLIT-HW-CC`, `:BLIT-HW-A`,
`:BLIT-SW`, `:BLIT-SW-CC`, `:BLIT-SW-A`or `:BLIT-FILL`. If `NIL`, returns a list of all supported
video flags.

##### Example

    \(video-info :HW-AVAILABLE\)"
  (let ((video-info (sdl-cffi::SDL-Get-Video-Info)))
    (unless (cffi:null-pointer-p video-info)
      (if info
        (remove nil (loop for attribute in info
                          collecting (find attribute (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::flags))))
        (cffi:foreign-slot-value video-info 'sdl-cffi::sdl-video-info 'sdl-cffi::flags)))))

(defun hw-available-p ()
  (video-info :hw-available))
(defun wm-available-p ()
  (video-info :wm-available))
(defun blit-hw-p ()
  (video-info :blit-hw))
(defun blit-hw-cc-p ()
  (video-info :blit-hw-cc))
(defun blit-hw-a-p ()
  (video-info :blit-hw-a))
(defun blit-sw-p ()
  (video-info :blit-sw))
(defun blit-sw-cc-p ()
  (video-info :blit-sw-cc))
(defun blit-sw-a-p ()
  (video-info :blit-sw-a))
(defun blit-fill-p ()
  (video-info :blit-fill))

(defun list-modes (&rest flags)
  "Returns a LIST of vectors sorted largest to smallest containing window or screen dimensions
that will support the specified video `FLAGS`. `LIST-MODES` must be called after SDL is 
initialised using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init).

##### Parameters

* `FLAGS` is one or more of the following; [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable) and [SDL-NO-FRAME](#sdl-no-frame).
* `SURFACE` A surface of type [SDL-SURFACE](#sdl-surface]), or `NIL`. WHEN `NIL`, the pixel format will be
that returned by [SDL-GET-VIDEO-INFO](#sdl-get-video-info]).

##### Returns

* Returns a list of `VECTOR`s of display dimensions, sorted largest to smallest, that will support 
the pixel format of surface `SURFACE`; for example `(#(1024 768) #(640 480) #(512 384) #(320 240))`.
Returns `NIL` if there are no dimensions available for a particular pixel format. 
Returns `T` if any dimension will support the pixel format and video flags.

##### Example

    \(LIST-MODES SDL-HW-SURFACE SDL-FULLSCREEN\)"
  (when (video-info)
    (let ((modes nil)
          (listmodes (sdl-cffi::SDL-List-Modes (cffi:null-pointer) (apply #'logior flags))))
    (cond
      ((cffi:null-pointer-p listmodes) nil)
      ((equal (cffi:pointer-address listmodes) 4294967295) t)
      (t
       (do ((i 0 (1+ i)))
             ((cffi:null-pointer-p (cffi:mem-aref listmodes :pointer i)) (reverse modes))
           (let ((rect (cffi:mem-ref (cffi:mem-aref listmodes :pointer i) 'sdl-cffi::sdl-rect)))
             (setf modes (cons (vector (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::w)
                                       (cffi:foreign-slot-value rect 'sdl-cffi::sdl-rect 'sdl-cffi::h))
                               modes)))))))))

(defun query-cursor ()
  "Queries the current state of the cursor. 
Returns `T` if the cursor is enabled and shown on the display. Returns `NIL` if the cursor 
is disabled and hidden."
  (case (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-query)
    (sdl-cffi::sdl-disable nil)
    (sdl-cffi::sdl-enable t)))

(defun video-driver-name ()
  "Returns the driver name of the initialised video driver. The driver name is a `STRING` containing a 
one-word identifier like \"x11\" or \"windib\". Returns 'NIL' if the video driver 
is not already initialised with [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init).

##### Example

    \(sdl:with-init \(\)
      \(sdl:video-driver-name\)\)
    >> \"windib\""
  (let ((string-return-val (cffi:with-foreign-pointer-as-string (str 100)
			     (sdl-cffi::sdl-video-driver-name str 100))))
    (if (equal string-return-val "")
	nil
	string-return-val)))

(defun set-gl-attribute (attribute value)
  (unless (video-init-p)
    (init-video))
  (sdl-cffi::sdl-gl-set-attribute attribute value))

(defun get-gl-attribute (attribute value)
  (declare (ignore attribute value))
  ;; TBD
  )

(defun set-caption (window-caption icon-caption)
  "Sets the caption text for window bar, and icon."
  (unless (cffi:null-pointer-p (sdl-cffi::SDL-Get-Video-Info))
    (sdl-cffi::sdl-wm-set-caption window-caption icon-caption)))

(defun get-caption ()
  "Returns the caption and icon text for the display surface as a spread; \(VALUES title-caption icon caption\)"
  (unless (cffi:null-pointer-p (sdl-cffi::SDL-Get-Video-Info))
    (cffi:with-foreign-objects ((title-handle :pointer)
                                (icon-handle :pointer))
    (sdl-cffi::SDL-WM-Get-Caption title-handle icon-handle)
    (let ((foreign-title-caption (cffi:mem-aref title-handle :pointer))
          (foreign-icon-caption (cffi:mem-aref icon-handle :pointer)))
      (values (cffi:foreign-string-to-lisp foreign-title-caption)
              (cffi:foreign-string-to-lisp foreign-icon-caption))))))
