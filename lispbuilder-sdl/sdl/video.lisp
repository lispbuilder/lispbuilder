;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Functions


(defun window (width height &key (bpp 0) (flags SDL-SW-SURFACE) title-caption icon-caption)
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
* `FLAGS` is a bitmasked logior of one or more of the following; `SDL-SW-SURFACE`, `SDL-HW-SURFACE`, `SDL-ASYNC-BLIT`,
`SDL-ANY-FORMAT`, `SDL-HW-PALETTE`, `SDL-DOUBLEBUF`, `SDL-FULLSCREEN`, `SDL-OPENGL`, `SDL-OPENGL-BLIT`, `SDL-RESIZABLE` 
AND `SDL-NO-FRAME`.
* `TITLE-CAPTION` is the title that appears in the Window title bar, of type `STRING`.
* `ICON-CAPTION` is the title that appears when the Window is minimized, of type `STRING`.

##### Surface Flags

The following section descibes the flags that can be passed to `WINDOW` in `FLAGS`.

* `SDL-SW-SURFACE`  Create the video surface in system memory.
* `SDL-HW-SURFACE`  Create the video surface in video memory.
* `SDL-ASYNC-BLIT`  Enables the use of asynchronous updates of the display surface. This will usually slow
 down blitting on single CPU machines, but may provide a speed increase on SMP systems.
* `SDL-ANY-FORMAT`  Normally, if a video surface of the requested `BPP` is not available, SDL will emulate
 one with a shadow surface. Passing `SDL-ANY-FORMAT` prevents this and causes SDL to use the video surface, 
regardless of its pixel depth.
* `SDL-HW-PALETTE`  Give SDL exclusive palette access. Without this flag you may
 not always get the the colors you request with [SDL-SET-COLORS](#sdl-set-colors) or [SDL-SET-PALETTE](#sdl-set-palette).
* `SDL-DOUBLEBUF`   Enable hardware double buffering; only valid with `SDL-HW-SURFACE`. 
Calling [UPDATE-DISPLAY](#update-display) will flip the buffers and update the screen. All drawing will 
take place on the surface that is not displayed at the moment. If double buffering could not be 
enabled then [UPDATE-DISPLAY](#update-display) will just perform a [SDL-UPDATE-RECT](#sdl-update-rect) on 
the entire screen.
* `SDL-FULLSCREEN`  SDL will attempt to use a fullscreen mode. If a hardware resolution change 
is not possible \(for whatever reason\), the next higher resolution will be used and the display window 
centered on a black background.
* `SDL-OPENGL`	    Create an OpenGL rendering context. You should have previously set OpenGL video attributes with 
[SDL-GL-SET-ATTRIBUTE](#sdl-gl-set-attribute).
* `SDL-RESIZABLE`   Create a resizable window. When the window is resized by the user a `:VIDEO-RESIZE` event is 
generated and `WINDOW` can be called again with the new size.
* `SDL-NO-FRAME`    If possible, `SDL-NO-FRAME` causes SDL to create a window with no title bar or frame decoration. 
Fullscreen modes automatically have this flag set.

##### Return

* Returns a new `DISPLAY-SURFACE` if successful, `NIL` if unsuccessful. Whatever flags SDL_SetVideoMode 
could satisfy are set in the flags member of `SURFACE`.
The `SURFACE` returned is freed by SDL and should never be freed by the caller.
This rule includes consecutive calls to `WINDOW` \(i.e. upon resize or resolution change\) - any existing surface 
will be released automatically by SDL.

##### Example

    \(WINDOW 320 240 :TITLE-CAPTION \"Random-Rects\" :ICON-CAPTION \"Random-Rects\"
                     :FLAGS \(SDL-DOUBLEBUF SDL-FULLSCREEN\)\)"
  (let ((surf (sdl-base::set-screen width height
				    :bpp bpp
				    :flags flags
				    :title-caption title-caption
				    :icon-caption icon-caption)))
    (setf *default-display* (surface surf t))))

(defun update-display (&optional (surface *default-display*))
  "Flips the buffers and updates the screen `SURFACE` if `SDL-HW-SURFACE` is set in [WINDOW](#window). 
If double buffering is not enabled then `UPDATE-DISPLAY` will perform a 
[SDL-UPDATE-RECT](#sdl-update-rect) on the entire screen.
`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified."
  (sdl-cffi::sdl-flip (fp surface)))

(defun clear-display (color &optional (surface *default-display*))
  "Fills the display `SURFACE` using color `COLOR`.
`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified."
  (sdl-base::fill-surface (fp surface)
			  (map-color color surface)))