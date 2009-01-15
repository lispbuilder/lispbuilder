;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Functions

(defun opengl-context-p (flags)
  "Returns `T` if [SDL-OPENGL](#sdl-opengl) is within the bitmask `FLAGS`, or returns `NIL` otherwise."
  (if (eq 1 (logand flags sdl-opengl))
      t
      nil))

(defun window (width height &key
                     (bpp 0) (flags SDL-SW-SURFACE) title-caption icon-caption
                     (fps (make-instance 'sdl-base::fps-fixed)))
  "Creates a new SDL window of pixel width `WIDTH` and height `HEIGHT` using SDL_SetVideoMode.

Use `SDL-SW-SURFACE` if you plan on doing per-pixel manipulations, or blit surfaces with alpha channels, 
and require a high framerate. When you use hardware surfaces like `SDL-HW-SURFACE`, SDL copies the surfaces 
from video memory to system memory when you lock them, and back when you unlock them. This can cause a major 
performance hit. \(Be aware that you may request a hardware surface, but receive a software surface. 
Many platforms can only provide a hardware surface when using `SDL-FULL-SCREEN.\) 
`SDL-HW-SURFACE` is best used when the surfaces you'll be blitting can also be stored in video memory.

*Note:* To control the position on the screen when creating a windowed surface, set the environment variables 
`SDL_VIDEO_CENTERED=center` or `SDL_VIDEO_WINDOW_POS=x,y`. These may be set using [SDL-PUT-ENV](#sdl-put-env).

##### Parameters

* `WIDTH` the pixel width of the window, of type `INTEGER`.
* `HEIGHT` the pixel height of the window, of type `INTEGER`.
If `WIDTH` and `HEIGHT` are both `0`, then the width and height of the current video mode is used 
\(or the desktop mode, if no mode has been set\).
* `BPP` the number of bits per pixel. Defaults to `0` which is the current display bits per pixel.
*Note:* A `BPP` of `24` uses the packed representation of 3 bytes/pixel. 
For the more common 4 bytes/pixel mode, use a `BPP` of 32.
* `FLAGS` is a bitmasked logior of one or more of the following; [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable) and [SDL-NO-FRAME](#SDL-NO-FRAME).
* `TITLE-CAPTION` is the title that appears in the Window title bar, of type `STRING`.
* `ICON-CAPTION` is the title that appears when the Window is minimized, of type `STRING`.


##### Returns

* Returns a new `DISPLAY-SURFACE` if successful, `NIL` if unsuccessful. Whatever flags SDL_SetVideoMode 
could satisfy are set in the flags member of `SURFACE`.
The `SURFACE` returned is freed by SDL and should never be freed by the caller.
This rule includes consecutive calls to `WINDOW` \(i.e. upon resize or resolution change\) - any existing surface 
will be released automatically by SDL.

##### Example

    \(WINDOW 320 240 :TITLE-CAPTION \"Random-Rects\" :ICON-CAPTION \"Random-Rects\"
                     :FLAGS \'(SDL-DOUBLEBUF SDL-FULLSCREEN\)\)"
  (when (= (sdl:return-subsystems-of-status SDL:SDL-INIT-VIDEO t) 0)
    (sdl:initialize-subsystems-on-startup (logior
                                           sdl::*initialize-subsystems-on-startup*
                                           SDL:SDL-INIT-VIDEO))
    (sdl:quit-subsystems-on-exit (logior
                                  sdl::*quit-subsystems-on-exit*
                                  SDL:SDL-INIT-VIDEO))
    ;; Initialize the subsystems in sdl::*initialize-subsystems-on-startup*
    ;; that are not yet initialized.
    (sdl:init-subsystems))
  (let ((surf (sdl-base::set-screen width height
				    :bpp bpp
				    :flags flags
				    :title-caption title-caption
				    :icon-caption icon-caption)))
    (setf *default-display* nil
	  *opengl-context* nil)
    (when surf
      (setf *default-display* (make-instance 'display-surface :fp surf))
      (setf *opengl-context* (surface-info *default-display* sdl-opengl)))
    (setf sdl-base::*default-fpsmanager* fps)
    *default-display*))


(defun update-display (&optional (surface *default-display*))
  "When [OPENGL-CONTEXT](#opengl-context) is `NIL`; `UPDATE-DISPLAY` will flip the SDL video buffers and update 
the screen `SURFACE` if `SDL-HW-SURFACE` is set in [WINDOW](#window). If double buffering is not enabled then
 SDL will perform an [SDL-UPDATE-RECT](#sdl-update-rect) on the entire screen.

When [OPENGL-CONTEXT](#opengl-context) is `T`; `UPDATE-DISPLAY` will call 
[SDL-GL-SWAP-BUFFERS](#sdl-gl-swap-buffers) to update the OpenGL display context.

`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified."
  (if *opengl-context*
      (sdl-cffi::sdl-gl-swap-buffers)
      (sdl-cffi::sdl-flip (fp surface))))

(defun clear-display (color &key (surface *default-display*) (update nil))
  "Fills the display `SURFACE` using color `COLOR`.
`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified. 
The display is updated when `UPDATE` is `T`."
  (sdl-base::fill-surface (fp surface)
			  (map-color color surface)
			  :update update
			  :clipping nil))

(defun show-cursor (state)
  "Disables the cursor when state is `NIL`, otherwise enables the cursor."
  (if state
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-enable)
      (sdl-cffi::SDL-Show-Cursor sdl-cffi::sdl-disable)))

(defun get-native-window ()
  "Returns a foreign pointer to the native SDL display window."
  (let ((wm-info (cffi:foreign-alloc 'sdl-cffi::SDL-Sys-WM-info)))
      ;; Set the wm-info structure to the current SDL version.
      (sdl-cffi::sdl-version (cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::version))
      (sdl-cffi::SDL-Get-WM-Info wm-info)
      ;; For Windows
      #+win32(cffi:foreign-slot-value wm-info 'sdl-cffi::SDL-Sys-WM-info 'sdl-cffi::window)
      ;; For X
      #-win32(cffi:foreign-slot-pointer (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer wm-info
											      'sdl-cffi::SDL-Sys-WM-info
											      'sdl-cffi::info)
								   'sdl-cffi::SDL-Sys-WM-info-info
								   'sdl-cffi::x11)
					'sdl-cffi::SDL-Sys-WM-info-info-x11
					'sdl-cffi::window)))

(defun surface-info (surface &optional (info nil))
  "Returns information about the SDL surface `SURFACE`.

##### Parameters

* `SURFACE` is an SDL surface of type [SDL-SURFACE](#sdl-surface).
* `INFO` must be one of `NIL`, [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable)
[SDL-HW-ACCEL](#sdl-hw-accel), [SDL-SRC-COLOR-KEY](#sdl-src-color-key),
[SDL-RLE-ACCEL](#sdl-rle-accel), [SDL-SRC-ALPHA](#sdl-src-alpha)
 or [SDL-PRE-ALLOC](#sdl-pre-alloc).

##### Returns

`INFO` when `NIL` will return a list of all enabled surface flags. Otherwise will
return `INFO` as `T` or `NIL` if supported by the surface.

##### Example

    \(SURFACE-INFO A-SURFACE '\(SDL-HW-SURFACE SDL-HW-PALETTE SDL-HW-ACCELL\)\)"
  (check-type surface sdl-surface)
  (if info
      (let ((property (find info (list SDL-HW-SURFACE SDL-SW-SURFACE SDL-ASYNC-BLIT SDL-ANY-FORMAT
				       SDL-HW-PALETTE SDL-DOUBLEBUF SDL-FULLSCREEN
				       SDL-OPENGL SDL-RESIZABLE SDL-HW-ACCEL
				       SDL-SRC-COLOR-KEY SDL-RLE-ACCEL SDL-SRC-ALPHA
				       SDL-PRE-ALLOC))))
	(if property
	    (if (eq (logand property
			    (cffi:foreign-slot-value (fp surface) 'sdl-cffi::sdl-surface 'sdl-cffi::flags))
		    property)
		t
		nil)))
      (remove nil (mapcar #'(lambda (query)
			      (let ((info (first query))
				    (description (second query)))
				(let ((result (logand (cffi:foreign-slot-value (fp surface) 'sdl-cffi::sdl-surface 'sdl-cffi::flags)
						      info)))
				  (unless (eq result 0)
				    description))))
			  (list (list SDL-HW-SURFACE 'SDL-HW-SURFACE)
				(list SDL-SW-SURFACE 'SDL-SW-SURFACE)
				(list SDL-ASYNC-BLIT 'SDL-ASYNC-BLIT)
				(list SDL-ANY-FORMAT 'SDL-ANY-FORMAT)
				(list SDL-HW-PALETTE 'SDL-HW-PALETTE)
				(list SDL-DOUBLEBUF 'SDL-DOUBLEBUF)
				(list SDL-FULLSCREEN 'SDL-FULLSCREEN)
				(list SDL-OPENGL 'SDL-OPENGL)
				(list SDL-RESIZABLE 'SDL-RESIZABLE)
				(list SDL-HW-ACCEL 'SDL-HW-ACCEL)
				(list SDL-SRC-COLOR-KEY 'SDL-SRC-COLOR-KEY)
				(list SDL-RLE-ACCEL 'SDL-RLE-ACCEL)
				(list SDL-SRC-ALPHA 'SDL-SRC-ALPHA)
				(list SDL-PRE-ALLOC 'SDL-PRE-ALLOC))))))

(defun video-memory ()
  "Returns the amount of video memory of the graphics hardware. Must be called after SDL is initialized 
using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)."
  (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::video-mem))

(defun video-dimensions ()
  "Returns the best video dimensions if called before a window is created, using [WINDOW](#window). 
Returns the current video dimensions if called after a window is created.
Must be called after SDL is initialized using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init)"
  (vector (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::current-w)
	  (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::current-h)))

(defun video-info (&optional (info nil))
  "Returns information about the video hardware. 
`GET-VIDEO-INFO` must be called after SDL is initialised using [INIT-SDL](#init-sdl) or 
[WITH-INIT](#with-init).
If `GET-VIDEO-INFO` is called before [WINDOW](#window), the information returned is of 
the *best* video mode. If `GET-VIDEO-INFO` is called after [WINDOW](#window), the information 
returned is of the *current* video mode. 

##### Parameters

* `INFO` can be one of `:HW-AVAILABLE`, `:WM-AVAILABLE`, `:BLIT-HW`, `:BLIT-HW-CC`, `:BLIT-HW-A`,
`:BLIT-SW`, `:BLIT-SW-CC`, `:BLIT-SW-A`or `:BLIT-FILL`. If `NIL`, returns a list of all supported
video flags.

##### Example

    \(video-info :HW-AVAILABLE\)"
  (if info
      (find info (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::flags))
      (cffi:foreign-slot-value (sdl-cffi::SDL-Get-Video-Info) 'sdl-cffi::sdl-video-info 'sdl-cffi::flags)))

(defun list-modes (flags &optional (surface nil))
  "Returns a LIST of vectors sorted largest to smallest that contains the width and height 
dimensions of the screen that will support the pixel format of the specified 
surface `SURFACE` and video flags `FLAGS`. `LIST-MODES` must be called after SDL is 
initialised using [INIT-SDL](#init-sdl) or [WITH-INIT](#with-init).

##### Parameters

* `FLAGS` is a bitmasked logior of one or more of the following; [SDL-SW-SURFACE](#sdl-sw-surface), 
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

    \(LIST-MODES '\(SDL-HW-SURFACE SDL-FULLSCREEN\)\)"
  (declare (ignore surface))
  (let ((modes nil)
        (listmodes (sdl-cffi::SDL-List-Modes (cffi:null-pointer) (sdl-base::set-flags flags))))
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
  (sdl-cffi::sdl-gl-set-attribute attribute value))

(defun set-caption (window-caption icon-caption)
  "Sets the caption text for window bar, and icon."
  (sdl-cffi::sdl-wm-set-caption window-caption icon-caption))