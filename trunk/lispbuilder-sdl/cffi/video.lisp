
(in-package #:lispbuilder-sdl-cffi) 

(defbitfield hardware-flags
  (:hw-available    #x0001)
  (:wm-available    #x0002)
  (:unused_bits_1   #x0004)
  (:unused_bits_2   #x0008)
  (:unused_bits_3   #x0010)  
  (:unused_bits_4   #x0020)
  (:unused_bits_5   #x0040)
  (:unused_bits_6   #x0080)
  (:unused_bits_7   #x0100)
  (:blit-hw         #x0200)
  (:blit-hw-CC      #x0400)
  (:blit-hw-A       #x0800)
  (:blit-sw         #x1000)
  (:blit-sw-CC      #x2000)
  (:blit-sw-A       #x4000)
  (:blit-fill       #x8000)
  (:unused_bits_1_1 #x10000)
  (:unused_bits_1_2 #x20000)
  (:unused_bits_1_3 #x40000)  
  (:unused_bits_1_4 #x80000)
  (:unused_bits_1_5 #x100000)
  (:unused_bits_1_6 #x200000)
  (:unused_bits_1_7 #x400000)
  (:unused_bits_1_8 #x800000)
  (:unused_bits_2_1 #x1000000)
  (:unused_bits_2_2 #x2000000)
  (:unused_bits_2_3 #x4000000)  
  (:unused_bits_2_4 #x8000000)
  (:unused_bits_2_5 #x10000000)
  (:unused_bits_2_6 #x20000000)
  (:unused_bits_2_7 #x40000000)
  (:unused_bits_2_8 #x80000000))

(cl:defconstant SDL-ALPHA-OPAQUE 255)

(cl:defconstant SDL-ALPHA-TRANSPARENT 0)

(cffi:defcstruct SDL-Rect
	(x :short)
	(y :short)
	(w :unsigned-short)
	(h :unsigned-short))

(cffi:defcstruct SDL-Color
	(r :unsigned-char)
	(g :unsigned-char)
	(b :unsigned-char)
	(unused :unsigned-char))

(cffi:defcstruct SDL-Palette
	(ncolors :int)
	(colors :pointer))

(cffi:defcstruct SDL-Pixel-Format
	(palette :pointer)
	(BitsPerPixel :unsigned-char)
	(BytesPerPixel :unsigned-char)
	(Rloss :unsigned-char)
	(Gloss :unsigned-char)
	(Bloss :unsigned-char)
	(Aloss :unsigned-char)
	(Rshift :unsigned-char)
	(Gshift :unsigned-char)
	(Bshift :unsigned-char)
	(Ashift :unsigned-char)
	(Rmask :unsigned-int)
	(Gmask :unsigned-int)
	(Bmask :unsigned-int)
	(Amask :unsigned-int)
	(colorkey :unsigned-int)
	(alpha :unsigned-char))

(cffi:defcstruct SDL-Surface
	(flags :unsigned-int)
	(format :pointer)
	(w :int)
	(h :int)
	(pitch :unsigned-short)
	(pixels :pointer)
	(offset :int)
	(hwdata :pointer)
	(clip-rect SDL-Rect)
	(unused1 :unsigned-int)
	(locked :unsigned-int)
	(map :pointer)
	(format-version :unsigned-int)
	(refcount :int))

(cl:defconstant SDL-SW-SURFACE #x00000000
  "Applies to [SURFACE](#surface) and [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window) or [CREATE-SURFACE](#create-surface); 
Create the [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface) in system memory.
* When read from the `FLAGS` field in [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface); 
Indicates if the surface is stored in system memory.

This improves the performance of pixel level access, however you may not be able to take advantage of some types of 
hardware blitting.")

(cl:defconstant SDL-HW-SURFACE #x00000001
  "Applies to [SURFACE](#surface) and [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window) or [CREATE-SURFACE](#create-surface); 
 Create the [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface) in video memory.
* When read from the `FLAGS` field in [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface); 
Indicates if the surface is stored in video memory.

This will allow SDL to take advantage of Video->Video blits (which are often accelerated).")

(cl:defconstant SDL-ASYNC-BLIT #x00000004
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
Enables the use of asynchronous updates of the [DISPLAY-SURFACE](#display-surface). 
* When read from the `FLAGS` field in [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface); 
Indicates if the surface uses asynchronous blits when possible.

Asynchronous blits usually slows down blitting on single CPU machines, but may provide a speed increase on SMP systems.")

(cl:defconstant SDL-ANY-FORMAT #x10000000
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
 Causes SDL to return a [DISPLAY-SURFACE](#display-surface) of any pixel depth if the requested pixel depth is unavailable.
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
Indicates that the surface was created with `SDL-ANY-FORMAT`.

Normally, if a [DISPLAY-SURFACE](#display-surface) of the requested bits-per-pixel (bpp) is not available, SDL 
will emulate one with a shadow surface. Passing `SDL-ANY-FORMAT` prevents this and causes SDL to use the
[DISPLAY-SURFACE](#display-surface) surface, regardless of its pixel depth.")

(cl:defconstant SDL-HW-PALETTE #x20000000
  "Applies to [SURFACE](#surface) and [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
Gives SDL exclusive palette access to [DISPLAY-SURFACE](#display-surface).
* When read from the `FLAGS` field in [SURFACE](#surface) or [DISPLAY-SURFACE](#display-surface); 
Indicates that the surface has an exclusive palette.

Without this flag you may not always get the the colors you request with [SDL-SET-COLORS](#sdl-set-colors) or 
[SDL-SET-PALETTE](#sdl-set-palette).")

(cl:defconstant SDL-DOUBLEBUF #x40000000
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
Enable hardware double buffering; only valid with [SDL-HW-SURFACE](#sdl-hw-surface). 
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
Indicages that the surface is double buffered.

Calling [SDL-FLIP](#sdl-flip) will flip the buffers and update the screen. All drawing will take place on the 
surface that is not displayed at the moment. If double buffering could not be enabled then [SDL-FLIP](#sdl-flip) 
will just perform a [SDL-UPDATE-RECT](#sdl-update-rect) on the entire screen.")

(cl:defconstant SDL-FULLSCREEN #x80000000
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
SDL will attempt to use a fullscreen mode. If a hardware resolution change is not possible (for whatever reason), 
the next higher resolution will be used and the display window centered on a black background.
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
Indicates that the surface is full-screen.")

(cl:defconstant SDL-OPENGL #x00000002
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
Create an OpenGL rendering context. You should have previously set OpenGL video attributes with 
[SDL-GL-SET-ATTRIBUTE](#sdl-gl-set-attribute).
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
Indicates that the surface has an OpenGL rendering context.")

(cl:defconstant SDL-RESIZABLE #x00000010
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
Create a resizable window.
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
Indicates that the surface is resizable.

When the window is resized by the user a :VIDEO-RESIZE-EVENT event is generated and [WINDOW](#window) 
can be called again with the new size.")

(cl:defconstant SDL-NO-FRAME #x00000020
  "Applies to [DISPLAY-SURFACE](#display-surface).
* When passed as a `FLAG` to [WINDOW](#window); 
If possible, `SDL-NO-FRAME` causes SDL to create a window with no title bar or frame decoration. 
* When read from the `FLAGS` field in [DISPLAY-SURFACE](#display-surface); 
__Note: Per the SDL documentation, this is not stored in `FLAGS`.

Fullscreen modes automatically have this flag set.")

(cl:defconstant SDL-HW-ACCEL #x00000100
  "Applies to [SURFACE](#surface) and [DISPLAY-SURFACE](#display-surface).
* When read from the `FLAGS` field in [SURFACE](#surface) and [DISPLAY-SURFACE](#display-surface); 
Indicates that surface blitting will use hardware acceloration.")

(cl:defconstant SDL-SRC-COLOR-KEY #x00001000
  "Applies to [SURFACE](#surface).
* When passed as a `FLAG` to [CREATE-SURFACE](#create-surface) and [SET-COLOR-KEY](#set-color-key); 
Turns on color keying for blits from this surface. If [SDL-HW-SURFACE](#sdl-hw-surface) is also specified and color 
keyed blits are hardware-accelerated, then SDL will attempt to place the surface in video memory. If the screen is a 
hardware surface and color keyed blits are hardware-accelerated then the [SDL-HW-SURFACE](#sdl-hw-surface) flag will be 
set. Use [SET-COLOR-KEY](#set-color-key) to set or clear this flag after surface creation.
* When read from the `FLAGS` field in [SURFACE](#surface); 
Indicates that the surface uses colorkey blitting.")

(cl:defconstant SDL-RLE-ACCEL-OK #x00002000
  "Applies to [SURFACE](#surface).
")

(cl:defconstant SDL-RLE-ACCEL #x00004000
  "Applies to [SURFACE](#surface)
* When passed as a `FLAG` to [SET-COLOR-KEY](#set-color-key); 
The surface will be drawn using RLE acceleration when drawn with [BLIT-SURFACE](#blit-surface), and 
[DRAW-SURFACE](#draw-surface). The surface will actually be encoded for RLE acceleration the first time 
[BLIT-SURFACE](#blit-surface), [DRAW-SURFACE](#draw-surface) or [CONVERT-SURFACE](#convert-surface)
 is called on the surface.
* When read from the `FLAGS` field in [SURFACE](#surface); 
Indicates that colorkey blitting is accelerated with RLE.

RLE acceleration can substantially speed up blitting of images with large horizontal runs of transparent pixels 
\(i.e., pixels that match the key color\). The key must be of the same pixel format as the surface,
[MAP-COLOR](#map-color) is often useful for obtaining an acceptable value.")

(cl:defconstant SDL-SRC-ALPHA #x00010000
  "Applies to [SURFACE](#surface).
* When passed as a `FLAG` to [CREATE-SURFACE](#create-surface) and [SET-ALPHA](#set-alpha); 
Turns on alpha-blending for blits from this surface. If [SDL-HW-SURFACE](#sdl-hw-surface) is also specified and 
alpha-blending blits are hardware-accelerated, then the surface will be placed in video memory if possible. If the 
screen is a hardware surface and alpha-blending blits are hardware-accelerated then the [SDL-HW-SURFACE](#sdl-hw-surface)
 flag will be set. Use [SET-ALPHA](#set-alpha) to set or clear this flag after surface creation.
* When read from the `FLAGS` field in [SURFACE](#surface); 
Indicates that surface blitting uses alpha blending.")

(cl:defconstant SDL-PRE-ALLOC #x01000000
  "Applies to [SURFACE](#surface).
* When read from the `FLAGS` field in [SURFACE](#surface); 
Indicates that surface uses preallocated memory.")

(cl:defconstant SDL-YV12-OVERLAY #x32315659)

(cl:defconstant SDL-IYUV-OVERLAY #x56555949)

(cl:defconstant SDL-YUY2-OVERLAY #x32595559)

(cl:defconstant SDL-UYVY-OVERLAY #x59565955)

(cl:defconstant SDL-YVYU-OVERLAY #x55595659)

(cffi:defcstruct SDL-Overlay
	(format :unsigned-int)
	(w :int)
	(h :int)
	(planes :int)
	(pitches :pointer)
	(pixels :pointer)
	(hwfuncs :pointer)
	(hwdata :pointer)
	(hw-overlay :unsigned-int)
	(UnusedBits :unsigned-int))

(cffi:defcenum SDL-GLattr
	:SDL-GL-RED-SIZE
	:SDL-GL-GREEN-SIZE
	:SDL-GL-BLUE-SIZE
	:SDL-GL-ALPHA-SIZE
	:SDL-GL-BUFFER-SIZE
	:SDL-GL-DOUBLEBUFFER
	:SDL-GL-DEPTH-SIZE
	:SDL-GL-STENCIL-SIZE
	:SDL-GL-ACCUM-RED-SIZE
	:SDL-GL-ACCUM-GREEN-SIZE
	:SDL-GL-ACCUM-BLUE-SIZE
	:SDL-GL-ACCUM-ALPHA-SIZE
	:SDL-GL-STEREO
	:SDL-GL-MULTISAMPLEBUFFERS
	:SDL-GL-MULTISAMPLESAMPLES
	:SDL-GL-ACCELERATED-VISUAL
	:SDL-GL-SWAP-CONTROL)

(cl:defconstant SDL-LOGPAL #x01)

(cl:defconstant SDL-PHYSPAL #x02)

(cffi:defcfun ("SDL_VideoInit" SDL-Video-Init) :int
  (driver-name :string)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_VideoQuit" SDL-Video-Quit) :void)

(cffi:defcfun ("SDL_VideoDriverName" sdl-Video-Driver-Name) :pointer
  (namebuf :pointer)
  (maxlen :int))

(cffi:defcfun ("SDL_GetVideoSurface" SDL-Get-Video-Surface) :pointer)

(cffi:defcfun ("SDL_GetVideoInfo" SDL-Get-Video-Info) :pointer)

(cffi:defcfun ("SDL_VideoModeOK" SDL-Video-Mode-OK) :int
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_ListModes" SDL-List-Modes) :pointer
  (format :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_SetVideoMode" SDL-Set-Video-Mode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_UpdateRects" SDL-Update-Rects) :void
  (screen :pointer)
  (numrects :int)
  (rects :pointer))

(cffi:defcfun ("SDL_UpdateRect" SDL-Update-Rect) :void
  (screen :pointer)
  (x :int)
  (y :int)
  (w :unsigned-int)
  (h :unsigned-int))

(cffi:defcfun ("SDL_Flip" SDL-Flip) :int
  (screen :pointer))

(cffi:defcfun ("SDL_SetGamma" SDL-Set-Gamma) :int
  (red :float)
  (green :float)
  (blue :float))

(cffi:defcfun ("SDL_SetGammaRamp" SDL-Set-Gamma-Ramp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("SDL_GetGammaRamp" SDL-Get-Gamma-Ramp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(cffi:defcfun ("SDL_SetColors" SDL-Set-Colors) :int
  (surface :pointer)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(cffi:defcfun ("SDL_SetPalette" SDL-Set-Palette) :int
  (surface :pointer)
  (flags :int)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(cffi:defcfun ("SDL_MapRGB" SDL-Map-RGB) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(cffi:defcfun ("SDL_MapRGBA" SDL-Map-RGBA) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(cffi:defcfun ("SDL_GetRGB" SDL-Get-RGB) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(cffi:defcfun ("SDL_GetRGBA" SDL-Get-RGBA) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(cffi:defcfun ("SDL_CreateRGBSurface" SDL-Create-RGB-Surface) :pointer
  (flags :unsigned-int)
  (width :int)
  (height :int)
  (depth :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(cffi:defcfun ("SDL_CreateRGBSurfaceFrom" SDL-Create-RGB-Surface-From) :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(cffi:defcfun ("SDL_FreeSurface" sdl-Free-Surface) :void
  (surface sdl-surface))

(cffi:defcfun ("SDL_LockSurface" SDL-Lock-Surface) :int
  (surface :pointer))

(cffi:defcfun ("SDL_UnlockSurface" SDL-Unlock-Surface) :void
  (surface :pointer))

(cffi:defcfun ("SDL_LoadBMP_RW" SDL-Load-BMP-RW) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("SDL_SaveBMP_RW" SDL-Save-BMP-RW) :int
  (surface :pointer)
  (dst :pointer)
  (freedst :int))

(cffi:defcfun ("SDL_SetColorKey" SDL-Set-Color-Key) :int
  (surface :pointer)
  (flag :unsigned-int)
  (key :unsigned-int))

(cffi:defcfun ("SDL_SetAlpha" SDL-Set-Alpha) :int
  (surface :pointer)
  (flag :unsigned-int)
  (alpha :unsigned-char))

(cffi:defcfun ("SDL_SetClipRect" sdl-Set-Clip-Rect) :void
  (surface sdl-surface)
  (rect sdl-rectangle))

(cffi:defcfun ("SDL_GetClipRect" sdl-Get-Clip-Rect) :void
  (surface sdl-surface)
  (rect sdl-rectangle))


(cffi:defcfun ("SDL_ConvertSurface" SDL-Convert-Surface) :pointer
  (src :pointer)
  (fmt :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("SDL_UpperBlit" sdl-Upper-Blit) :int
  (src sdl-surface)
  (srcrect sdl-rectangle)
  (dst sdl-surface)
  (dstrect sdl-rectangle))

(cffi:defcfun ("SDL_LowerBlit" SDL-Lower-Blit) :int
  (src sdl-surface)
  (srcrect sdl-rectangle)
  (dst sdl-surface)
  (dstrect sdl-rectangle))

(cffi:defcfun ("SDL_FillRect" sdl-Fill-Rect) :int
  (dst sdl-surface)
  (dstrect sdl-rectangle)
  (color :unsigned-int))

(cffi:defcfun ("SDL_DisplayFormat" SDL-Display-Format) :pointer
  (surface :pointer))

(cffi:defcfun ("SDL_DisplayFormatAlpha" SDL-Display-Format-Alpha) :pointer
  (surface :pointer))

(cffi:defcfun ("SDL_CreateYUVOverlay" SDL-Create-YUV-Overlay) :pointer
  (width :int)
  (height :int)
  (format :unsigned-int)
  (display :pointer))

(cffi:defcfun ("SDL_LockYUVOverlay" SDL-Lock-YUV-Overlay) :int
  (overlay :pointer))

(cffi:defcfun ("SDL_UnlockYUVOverlay" SDL-Unlock-YUV-Overlay) :void
  (overlay :pointer))

(cffi:defcfun ("SDL_DisplayYUVOverlay" SDL-Display-YUV-Overlay) :int
  (overlay :pointer)
  (dstrect :pointer))

(cffi:defcfun ("SDL_FreeYUVOverlay" SDL-Free-YUV-Overlay) :void
  (overlay :pointer))

(cffi:defcfun ("SDL_GL_LoadLibrary" SDL-GL-Load-Library) :int
  (path :string))

(cffi:defcfun ("SDL_GL_GetProcAddress" SDL-GL-Get-Proc-Address) :pointer
  (proc :string))

(cffi:defcfun ("SDL_GL_SetAttribute" SDL-GL-Set-Attribute) :int
  (attr SDL-GLattr)
  (value :int))

(cffi:defcfun ("SDL_GL_GetAttribute" SDL-GL-Get-Attribute) :int
  (attr SDL-GLattr)
  (value :pointer))

(cffi:defcfun ("SDL_GL_SwapBuffers" SDL-GL-Swap-Buffers) :void)

(cffi:defcfun ("SDL_GL_UpdateRects" SDL-GL-Update-Rects) :void
  (numrects :int)
  (rects :pointer))

(cffi:defcfun ("SDL_GL_Lock" SDL-GL-Lock) :void)

(cffi:defcfun ("SDL_GL_Unlock" SDL-GL-Unlock) :void)

(cffi:defcfun ("SDL_WM_SetCaption" sdl-WM-Set-Caption) :void
  (title sdl-string)
  (icon sdl-string))

(cffi:defcfun ("SDL_WM_GetCaption" SDL-WM-Get-Caption) :void
  (title :pointer)
  (icon :pointer))

(cffi:defcfun ("SDL_WM_SetIcon" SDL-WM-Set-Icon) :void
  (icon :pointer)
  (mask :pointer))

(cffi:defcfun ("SDL_WM_IconifyWindow" SDL-WM-Iconify-Window) :int)

(cffi:defcfun ("SDL_WM_ToggleFullScreen" SDL-WM-Toggle-Full-Screen) :int
  (surface :pointer))

(cffi:defcenum SDL-Grab-Mode
	(:SDL-GRAB-QUERY -1)
	(:SDL-GRAB-OFF 0)
	(:SDL-GRAB-ON 1)
	:SDL-GRAB-FULLSCREEN)

(cffi:defcfun ("SDL_WM_GrabInput" SDL-WM-Grab-Input) SDL-Grab-Mode
  (mode SDL-Grab-Mode))

(cffi:defcfun ("SDL_SoftStretch" SDL-Soft-Stretch) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))


(defcstruct SDL-Video-Info
  (flags hardware-flags)
  (video-mem :unsigned-int)
  (vfmt :pointer)
  (current-w :int)	;; New for SDL-1.2.11
  (current-h :int))	;; New for SDL-1.2.11


(defun SDL-Load-BMP (file)
  (SDL-Load-BMP-RW (sdl-RW-From-File file "rb") 1))

;; (defun SDL_AllocSurface ()
;;   (SDL_nCreateRGBSurface))

(defun SDL-Save-BMP (surface file)
  (SDL-Save-BMP-RW surface (SDL-RW-FROM-FILE file "wb") 1))

(defun SDL-Blit-Surface (src srcrect dst dstrect)
  (SDL-Upper-Blit src srcrect dst dstrect))

