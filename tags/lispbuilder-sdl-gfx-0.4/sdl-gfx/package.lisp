;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx
  (:use #:cl #:cffi)
  (:nicknames #:sdl-gfx)
  (:documentation "The main package of `lispbuilder-gfx-sdl'.")
  ;; SDL_gfx
  ;;
  (:export #:gfxPrimitivesFontdata
	   #:SDL_imageFilterBinarizeUsingThreshold
	   #:FPS_UPPER_LIMIT
	   #:FPS_LOWER_LIMIT
	   #:FPS_DEFAULT
	   #:SDL_initFramerate
	   #:SDL_setFramerate
	   #:SDL_getFramerate
	   #:SDL_framerateDelay
	   #:M_PI
	   #:SMOOTHING_OFF
	   #:SMOOTHING_ON
	   #:tColorRGBA
	   #:tColorY
	   #:rotozoomSurface
	   #:rotozoomSurfaceXY
	   #:rotozoomSurfaceSize
	   #:rotozoomSurfaceSizeXY
	   #:zoomSurface
	   #:zoomSurfaceSize
	   #:SDL_imageFilterMMXdetect
	   #:SDL_imageFilterMMXoff
	   #:SDL_imageFilterMMXon
	   #:SDL_imageFilterAdd
	   #:SDL_imageFilterMean
	   #:SDL_imageFilterSub
	   #:SDL_imageFilterAbsDiff
	   #:SDL_imageFilterMult
	   #:SDL_imageFilterMultNor
	   #:SDL_imageFilterMultDivby2
	   #:SDL_imageFilterMultDivby4
	   #:SDL_imageFilterBitAnd
	   #:SDL_imageFilterBitOr
	   #:SDL_imageFilterDiv
	   #:SDL_imageFilterBitNegatio
	   #:SDL_imageFilterAddByte
	   #:SDL_imageFilterAddUint
	   #:SDL_imageFilterAddByteToHalf
	   #:SDL_imageFilterSubByte
	   #:SDL_imageFilterSubUint
	   #:SDL_imageFilterShiftRight
	   #:SDL_imageFilterShiftRightUint
	   #:SDL_imageFilterMultByByte
	   #:SDL_imageFilterShiftRightAndMultByByte
	   #:SDL_imageFilterShiftLeftByte
	   #:SDL_imageFilterShiftLeftUint
	   #:SDL_imageFilterShiftLeft
	   #:SDL_imageFilterClipToRange
	   #:SDL_imageFilterNormalizeLinear
	   #:SDL_imageFilterConvolveKernel3x3Divide
	   #:SDL_imageFilterConvolveKernel5x5Divide
	   #:SDL_imageFilterConvolveKernel7x7Divide
	   #:SDL_imageFilterConvolveKernel9x9Divide
	   #:SDL_imageFilterConvolveKernel3x3ShiftRight
	   #:SDL_imageFilterConvolveKernel5x5ShiftRight
	   #:SDL_imageFilterConvolveKernel7x7ShiftRight
	   #:SDL_imageFilterConvolveKernel9x9ShiftRight
	   #:SDL_imageFilterSobelX
	   #:SDL_imageFilterSobelXShiftRight
	   #:SDL_imageFilterAlignStack
	   #:SDL_imageFilterRestoreStack
	   #:GFX_FONTDATAMAX
	   #:SDL_GFXPRIMITIVES_MAJOR
	   #:SDL_GFXPRIMITIVES_MINOR
	   #:SDL_GFXPRIMITIVES_MICRO
	   #:pixelColor
	   #:pixelRGBA
	   #:hlineColor
	   #:hlineRGBA
	   #:vlineColor
	   #:vlineRGBA
	   #:rectangleColor
	   #:rectangleRGBA
	   #:boxColor
	   #:boxRGBA
	   #:lineColor
	   #:lineRGBA
	   #:aalineColor
	   #:aalineRGBA
	   #:circleColor
	   #:circleRGBA
	   #:aacircleColor
	   #:aacircleRGBA
	   #:filledCircleColor
	   #:filledCircleRGBA
	   #:ellipseColor
	   #:ellipseRGBA
	   #:aaellipseColor
	   #:aaellipseRGBA
	   #:filledEllipseColor
	   #:filledEllipseRGBA
	   #:pieColor
	   #:pieRGBA
	   #:filledPieColor
	   #:filledPieRGBA
	   #:trigonColor
	   #:trigonRGBA
	   #:aatrigonColor
	   #:aatrigonRGBA
	   #:filledTrigonColor
	   #:filledTrigonRGBA
	   #:polygonColor
	   #:polygonRGBA
	   #:aapolygonColor
	   #:aapolygonRGB
	   #:filledPolygonColor
	   #:filledPolygonRGBA
	   #:bezierColor
	   #:bezierRGBA
	   #:characterColor
	   #:characterRGBA
	   #:stringColor
	   #:stringRGBA
	   #:gfxPrimitivesSetFont
	   ;; util-sdl_gfx
))

