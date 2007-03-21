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
* `FLAGS` is a bitmasked logior of one or more of the following; [SDL-SW-SURFACE](#sdl-sw-surface), 
[SDL-HW-SURFACE](#sdl-hw-surface), [SDL-ASYNC-BLIT](#sdl-async-blit),
[SDL-ANY-FORMAT](#sdl-any-format), [SDL-HW-PALETTE](#sdl-hw-palette), 
[SDL-DOUBLEBUF](#sdl-doublebuf), [SDL-FULLSCREEN](#sdl-fullscreen), 
[SDL-OPENGL](#sdl-opengl), [SDL-RESIZABLE](#sdl-resizable) and [SDL-NO-FRAME](#SDL-NO-FRAME).
* `TITLE-CAPTION` is the title that appears in the Window title bar, of type `STRING`.
* `ICON-CAPTION` is the title that appears when the Window is minimized, of type `STRING`.


##### Return

* Returns a new `DISPLAY-SURFACE` if successful, `NIL` if unsuccessful. Whatever flags SDL_SetVideoMode 
could satisfy are set in the flags member of `SURFACE`.
The `SURFACE` returned is freed by SDL and should never be freed by the caller.
This rule includes consecutive calls to `WINDOW` \(i.e. upon resize or resolution change\) - any existing surface 
will be released automatically by SDL.

##### Example

    \(WINDOW 320 240 :TITLE-CAPTION \"Random-Rects\" :ICON-CAPTION \"Random-Rects\"
                     :FLAGS \'(SDL-DOUBLEBUF SDL-FULLSCREEN\)\)"
  (let ((surf (sdl-base::set-screen width height
				    :bpp bpp
				    :flags flags
				    :title-caption title-caption
				    :icon-caption icon-caption)))
    (if surf
	(setf *default-display* (surface surf t)
	      *opengl-context* (opengl-context-p (sdl-base::set-flags flags)))
	(setf *default-display* nil
	      *opengl-context* nil))
    surf))


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

(defun clear-display (color &optional (surface *default-display*))
  "Fills the display `SURFACE` using color `COLOR`.
`SURFACE` is bound to `\*DEFAULT-DISPLAY*\` if unspecified."
  (sdl-base::fill-surface (fp surface)
			  (map-color color surface)))