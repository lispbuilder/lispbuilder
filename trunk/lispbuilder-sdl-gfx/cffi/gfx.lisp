;; SDL_ttf2.0.13 CFFI lisp wrapper
;; SDL_gfx library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license

(in-package #:lispbuilder-sdl-gfx-cffi)

(defun return-true-to-1 (value) (if value 1 0))
(defun return-int-to-double (value) (coerce value 'double-float))

(defctype true-to-1 (:wrapper :int :to-c return-true-to-1))
(defctype int-to-double (:wrapper :double :to-c return-int-to-double))


(defconstant FPS-UPPER-LIMIT 200)

(defconstant FPS-LOWER-LIMIT 1)

(defconstant FPS-DEFAULT 30)

(defcstruct FPS-manager
	(framecount :uint32)
	(rateticks :float)
	(lastticks :uint32)
	(rate :uint32))

(defcfun ("SDL_initFramerate" SDL-init-Frame-rate) :void
  (manager :pointer))

(defcfun ("SDL_setFramerate" SDL-set-Frame-rate) :int
  (manager :pointer)
  (rate :int))

(defcfun ("SDL_getFramerate" SDL-get-Frame-rate) :int
  (manager :pointer))

(defcfun ("SDL_framerateDelay" SDL-frame-rate-Delay) :void
  (manager :pointer))

(defconstant M-PI 3.141592654)

(defconstant SMOOTHING-OFF 0)

(defconstant SMOOTHING-ON 1)

(defcstruct t-Color-RGBA
	(r :uint8)
	(g :uint8)
	(b :uint8)
	(a :uint8))

(defcstruct t-Color-Y
	(y :uint8))

(defctype sdl-gfx-font-data :pointer)

(defcfun ("rotozoomSurface" rotozoomSurface) :pointer
  (src sdl-cffi::sdl-surface)
  (angle int-to-double)
  (zoom int-to-double)
  (smooth true-to-1))

(defcfun ("rotozoomSurfaceXY" rotozoomSurfaceXY) :pointer
  (src sdl-cffi::sdl-surface)
  (angle int-to-double)
  (zoomx int-to-double)
  (zoomy int-to-double)
  (smooth true-to-1))

(defcfun ("rotozoomSurfaceSize" rotozoomSurfaceSize) :void
  (width :int)
  (height :int)
  (angle int-to-double)
  (zoom int-to-double)
  (dstwidth :pointer)
  (dstheight :pointer))

(defcfun ("rotozoomSurfaceSizeXY" rotozoomSurfaceSizeXY) :void
  (width :int)
  (height :int)
  (angle int-to-double)
  (zoomx int-to-double)
  (zoomy int-to-double)
  (dstwidth :pointer)
  (dstheight :pointer))

(defcfun ("zoomSurface" zoomSurface) :pointer
  (src sdl-cffi::sdl-surface)
  (zoomx int-to-double)
  (zoomy int-to-double)
  (smooth true-to-1))

(defcfun ("zoomSurfaceSize" zoomSurfaceSize) :void
  (width :int)
  (height :int)
  (zoomx int-to-double)
  (zoomy int-to-double)
  (dstwidth :pointer)
  (dstheight :pointer))

;; SDL_gfx 2.0.16
;; (defcfun ("shrinkSurface" shrinkSurface) :pointer
;;   (src sdl-cffi::sdl-surface)
;;   (factorx :int)
;;   (factory :int))


(defcfun ("SDL_imageFilterMMXdetect" SDL-image-Filter-MMX-detect) :int)

(defcfun ("SDL_imageFilterMMXoff" SDL-image-Filter-MMX-off) :void)

(defcfun ("SDL_imageFilterMMXon" SDL-image-Filter-MMX-on) :void)

(defcfun ("SDL_imageFilterAdd" SDL-image-Filter-Add) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterMean" SDL-image-Filter-Mean) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterSub" SDL-image-Filter-Sub) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterAbsDiff" SDL-image-Filter-Abs-Diff) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterMult" SDL-image-Filter-Mult) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterMultNor" SDL-image-Filter-Mult-Nor) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterMultDivby2" SDL-image-Filter-Mult-Div-by-2) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterMultDivby4" SDL-image-Filter-Mult-Div-by-4) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterBitAnd" SDL-image-Filter-Bit-And) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterBitOr" SDL-image-Filter-Bit-Or) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterDiv" SDL-image-Filter-Div) :int
  (Src1 :pointer)
  (Src2 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterBitNegation" SDL-image-Filter-Bit-Negation) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int))

(defcfun ("SDL_imageFilterAddByte" SDL-image-Filter-Add-Byte) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-char))

(defcfun ("SDL_imageFilterAddUint" SDL-image-Filter-Add-Uint) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-int))

(defcfun ("SDL_imageFilterAddByteToHalf" SDL-image-Filter-Add-Byte-To-Half) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-char))

(defcfun ("SDL_imageFilterSubByte" SDL-image-Filter-Sub-Byte) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-char))

(defcfun ("SDL_imageFilterSubUint" SDL-image-Filter-Sub-Uint) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-int))

(defcfun ("SDL_imageFilterShiftRight" SDL-image-Filter-Shift-Right) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char))

(defcfun ("SDL_imageFilterShiftRightUint" SDL-image-Filter-Shift-Right-Uint) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char))

(defcfun ("SDL_imageFilterMultByByte" SDL-image-Filter-Mult-By-Byte) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (C :unsigned-char))

(defcfun ("SDL_imageFilterShiftRightAndMultByByte" SDL-image-Filter-Shift-Right-And-Mult-By-Byte) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char)
  (C :unsigned-char))

(defcfun ("SDL_imageFilterShiftLeftByte" SDL-image-Filter-Shift-Left-Byte) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char))

(defcfun ("SDL_imageFilterShiftLeftUint" SDL-image-Filter-Shift-Left-Uint) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char))

(defcfun ("SDL_imageFilterShiftLeft" SDL-image-Filter-Shift-Left) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (N :unsigned-char))

(defcfun ("SDL_imageFilterBinarizeUsingThreshold" SDL-image-Filter-Binarize-Using-Threshold) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (t-arg3 :unsigned-char))

(defcfun ("SDL_imageFilterClipToRange" SDL-image-Filter-Clip-To-Range) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (Tmin :unsigned-char)
  (Tmax :unsigned-char))

(defcfun ("SDL_imageFilterNormalizeLinear" SDL-image-Filter-Normalize-Linear) :int
  (Src1 :pointer)
  (Dest :pointer)
  (length :int)
  (Cmin :int)
  (Cmax :int)
  (Nmin :int)
  (Nmax :int))

(defcfun ("SDL_imageFilterConvolveKernel3x3Divide" SDL-image-Filter-Convolve-Kernel-3x3-Divide) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (Divisor :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel5x5Divide" SDL-image-Filter-Convolve-Kerne-l5x5-Divide) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (Divisor :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel7x7Divide" SDL-image-Filter-Convolve-Kernel-7x7-Divide) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (Divisor :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel9x9Divide" SDL-image-Filter-Convolve-Kernel-9x9-Divide) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (Divisor :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel3x3ShiftRight" SDL-image-Filter-Convolve-Kernel-3x3-Shift-Right) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (NRightShift :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel5x5ShiftRight" SDL-image-Filter-Convolve-Kernel-5x5-Shift-Right) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (NRightShift :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel7x7ShiftRight" SDL-image-Filter-Convolve-Kernel-7x7-Shift-Right) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (NRightShift :unsigned-char))

(defcfun ("SDL_imageFilterConvolveKernel9x9ShiftRight" SDL-image-Filter-Convolve-Kernel-9x9-Shift-Right) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (Kernel :pointer)
  (NRightShift :unsigned-char))

(defcfun ("SDL_imageFilterSobelX" SDL-image-Filter-Sobel-X) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int))

(defcfun ("SDL_imageFilterSobelXShiftRight" SDL-image-Filter-Sobel-X-Shift-Right) :int
  (Src :pointer)
  (Dest :pointer)
  (rows :int)
  (columns :int)
  (NRightShift :unsigned-char))

(defcfun ("SDL_imageFilterAlignStack" SDL-image-Filter-Align-Stack) :void)

(defcfun ("SDL_imageFilterRestoreStack" SDL-image-Filter-Restore-Stack) :void)

(defconstant GFX-FONT-DATA-MAX 2048)

(defconstant SDL-GFX-PRIMITIVES-MAJOR 2)

(defconstant SDL-GFX-PRIMITIVES-MINOR 0)

(defconstant SDL-GFX-PRIMITIVES-MICRO 13)
;; SDL_gfx 2.0.16
;; (defconstant SDL-GFX-PRIMITIVES-MICRO 16)


(defcfun ("pixelColor" pixel-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (color :uint32))

(defcfun ("pixelRGBA" pixel-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("hlineColor" hline-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (x2 :int16)
  (y :int16)
  (color :uint32))

(defcfun ("hlineRGBA" hline-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (x2 :int16)
  (y :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("vlineColor" vline-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y1 :int16)
  (y2 :int16)
  (color :uint32))

(defcfun ("vlineRGBA" vline-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y1 :int16)
  (y2 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("rectangleColor" rectangle-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (color :uint32))

(defcfun ("rectangleRGBA" rectangle-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("boxColor" box-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (color :uint32))

(defcfun ("boxRGBA" box-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("lineColor" line-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (color :uint32))

(defcfun ("lineRGBA" line-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("aalineColor" aa-line-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (color :uint32))

(defcfun ("aalineRGBA" aa-line-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("circleColor" circle-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (r :int16)
  (color :uint32))

(defcfun ("circleRGBA" circle-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("aacircleColor" aa-circle-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (r :int16)
  (color :uint32))

(defcfun ("aacircleRGBA" aa-circle-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("filledCircleColor" filled-Circle-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (r :int16)
  (color :uint32))

(defcfun ("filledCircleRGBA" filled-Circle-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("ellipseColor" ellipse-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rx :int16)
  (ry :int16)
  (color :uint32))

(defcfun ("ellipseRGBA" ellipse-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rx :int16)
  (ry :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("aaellipseColor" aa-ellipse-Color) :int
  (dst sdl-cffi::sdl-surface)
  (xc :int16)
  (yc :int16)
  (rx :int16)
  (ry :int16)
  (color :uint32))

(defcfun ("aaellipseRGBA" aa-ellipse-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rx :int16)
  (ry :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("filledEllipseColor" filled-Ellipse-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rx :int16)
  (ry :int16)
  (color :uint32))

(defcfun ("filledEllipseRGBA" filled-Ellipse-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rx :int16)
  (ry :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("pieColor" pie-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (start :int16)
  (end :int16)
  (color :uint32))

(defcfun ("pieRGBA" pie-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (start :int16)
  (end :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("filledPieColor" filled-Pie-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (start :int16)
  (end :int16)
  (color :uint32))

(defcfun ("filledPieRGBA" filled-Pie-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (rad :int16)
  (start :int16)
  (end :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("trigonColor" trigon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (color :uint32))

(defcfun ("trigonRGBA" trigon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("aatrigonColor" aa-trigon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (color :uint32))

(defcfun ("aatrigonRGBA" aa-trigon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("filledTrigonColor" filled-Trigon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (color :uint32))

(defcfun ("filledTrigonRGBA" filled-Trigon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x1 :int16)
  (y1 :int16)
  (x2 :int16)
  (y2 :int16)
  (x3 :int16)
  (y3 :int16)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("polygonColor" polygon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (color :uint32))

(defcfun ("polygonRGBA" polygon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("aapolygonColor" aa-polygon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (color :uint32))

(defcfun ("aapolygonRGBA" aa-polygon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("filledPolygonColor" filled-Polygon-Color) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (color :uint32))

(defcfun ("filledPolygonRGBA" filled-Polygon-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

;; SDL_gfx 2.0.16
;; (defcfun ("texturedPolygon" textured-Polygon) :int
;;   (dst sdl-cffi::sdl-surface)
;;   (vx :pointer)
;;   (vy :pointer)
;;   (n :int)
;;   (texture sdl-cffi::sdl-surface)
;;   (texture-dx int)
;;   (texture-dy int))

(defcfun ("bezierColor" bezier-Color) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (s :int)
  (color :uint32))

(defcfun ("bezierRGBA" bezier-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (vx :pointer)
  (vy :pointer)
  (n :int)
  (s :int)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("characterColor" character-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (c :char)
  (color :uint32))

(defcfun ("characterRGBA" character-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (c :char)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("stringColor" string-Color) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (c :string)
  (color :uint32))

(defcfun ("stringRGBA" string-RGBA) :int
  (dst sdl-cffi::sdl-surface)
  (x :int16)
  (y :int16)
  (c :string)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defcfun ("gfxPrimitivesSetFont" gfx-Primitives-Set-Font) :void
  "Sets the font to use. Only a single font is supported at any one time. 
Subsequent calls to gfx-Primitives-Set-Font will replace the previous font.
No function exists to free the font data."
  (fontdata sdl-gfx-font-data)
  (cw :int)
  (ch :int))

;; SDL_gfx 2.0.16



;; (defmethod translate-to-foreign (value (type (eql 'sdl-gfx-font-data)))
;;   (values (cffi:foreign-alloc :unsigned-char
;; 			      :initial-contents (loop for i in value
;; 						   collect (parse-integer i
;; 									  :junk-allowed nil
;; 									  :start 2
;; 									  :radix 16)))
;; 	  t))

;; (defmethod free-translated-object (ptr (name (eql 'sdl-gfx-font-data)) free-p)
;;   (if free-p
;;       (cffi:foreign-free ptr)))
