;;; -*- lisp -*-

(in-package :common-lisp-user)

(defpackage :lispbuilder-sdl-gfx
  (:nicknames :sdl-gfx)
  (:use :common-lisp :cffi :lispbuilder-sdl)
  (:documentation "The main package of `lispbuilder-gfx-sdl'.")
  ;; SDL_gfx
  ;;
  (:export :gfxPrimitivesFontdata)
  (:export :SDL_imageFilterBinarizeUsingThreshold)
  (:export :FPS_UPPER_LIMIT)
  (:export :FPS_LOWER_LIMIT)
  (:export :FPS_DEFAULT)
  (:export :SDL_initFramerate)
  (:export :SDL_setFramerate)
  (:export :SDL_getFramerate)
  (:export :SDL_framerateDelay)
  (:export :M_PI)
  (:export :SMOOTHING_OFF)
  (:export :SMOOTHING_ON)
  (:export :tColorRGBA)
  (:export :tColorY)
  (:export :rotozoomSurface)
  (:export :rotozoomSurfaceXY)
  (:export :rotozoomSurfaceSize)
  (:export :rotozoomSurfaceSizeXY)
  (:export :zoomSurface)
  (:export :zoomSurfaceSize)
  (:export :SDL_imageFilterMMXdetect)
  (:export :SDL_imageFilterMMXoff)
  (:export :SDL_imageFilterMMXon)
  (:export :SDL_imageFilterAdd)
  (:export :SDL_imageFilterMean)
  (:export :SDL_imageFilterSub)
  (:export :SDL_imageFilterAbsDiff)
  (:export :SDL_imageFilterMult)
  (:export :SDL_imageFilterMultNor)
  (:export :SDL_imageFilterMultDivby2)
  (:export :SDL_imageFilterMultDivby4)
  (:export :SDL_imageFilterBitAnd)
  (:export :SDL_imageFilterBitOr)
  (:export :SDL_imageFilterDiv)
  (:export :SDL_imageFilterBitNegation)
  (:export :SDL_imageFilterAddByte)
  (:export :SDL_imageFilterAddUint)
  (:export :SDL_imageFilterAddByteToHalf)
  (:export :SDL_imageFilterSubByte)
  (:export :SDL_imageFilterSubUint)
  (:export :SDL_imageFilterShiftRight)
  (:export :SDL_imageFilterShiftRightUint)
  (:export :SDL_imageFilterMultByByte)
  (:export :SDL_imageFilterShiftRightAndMultByByte)
  (:export :SDL_imageFilterShiftLeftByte)
  (:export :SDL_imageFilterShiftLeftUint)
  (:export :SDL_imageFilterShiftLeft)
  (:export :SDL_imageFilterClipToRange)
  (:export :SDL_imageFilterNormalizeLinear)
  (:export :SDL_imageFilterConvolveKernel3x3Divide)
  (:export :SDL_imageFilterConvolveKernel5x5Divide)
  (:export :SDL_imageFilterConvolveKernel7x7Divide)
  (:export :SDL_imageFilterConvolveKernel9x9Divide)
  (:export :SDL_imageFilterConvolveKernel3x3ShiftRight)
  (:export :SDL_imageFilterConvolveKernel5x5ShiftRight)
  (:export :SDL_imageFilterConvolveKernel7x7ShiftRight)
  (:export :SDL_imageFilterConvolveKernel9x9ShiftRight)
  (:export :SDL_imageFilterSobelX)
  (:export :SDL_imageFilterSobelXShiftRight)
  (:export :SDL_imageFilterAlignStack)
  (:export :SDL_imageFilterRestoreStack)
  (:export :GFX_FONTDATAMAX)
  (:export :SDL_GFXPRIMITIVES_MAJOR)
  (:export :SDL_GFXPRIMITIVES_MINOR)
  (:export :SDL_GFXPRIMITIVES_MICRO)
  (:export :pixelColor)
  (:export :pixelRGBA)
  (:export :hlineColor)
  (:export :hlineRGBA)
  (:export :vlineColor)
  (:export :vlineRGBA)
  (:export :rectangleColor)
  (:export :rectangleRGBA)
  (:export :boxColor)
  (:export :boxRGBA)
  (:export :lineColor)
  (:export :lineRGBA)
  (:export :aalineColor)
  (:export :aalineRGBA)
  (:export :circleColor)
  (:export :circleRGBA)
  (:export :aacircleColor)
  (:export :aacircleRGBA)
  (:export :filledCircleColor)
  (:export :filledCircleRGBA)
  (:export :ellipseColor)
  (:export :ellipseRGBA)
  (:export :aaellipseColor)
  (:export :aaellipseRGBA)
  (:export :filledEllipseColor)
  (:export :filledEllipseRGBA)
  (:export :pieColor)
  (:export :pieRGBA)
  (:export :filledPieColor)
  (:export :filledPieRGBA)
  (:export :trigonColor)
  (:export :trigonRGBA)
  (:export :aatrigonColor)
  (:export :aatrigonRGBA)
  (:export :filledTrigonColor)
  (:export :filledTrigonRGBA)
  (:export :polygonColor)
  (:export :polygonRGBA)
  (:export :aapolygonColor)
  (:export :aapolygonRGBA)
  (:export :filledPolygonColor)
  (:export :filledPolygonRGBA)
  (:export :bezierColor)
  (:export :bezierRGBA)
  (:export :characterColor)
  (:export :characterRGBA)
  (:export :stringColor)
  (:export :stringRGBA)
  (:export :gfxPrimitivesSetFont)
  ;; util-sdl_gfx
  ;;
  (:export :*sdl-gfx-binaries-load-path*)
  (:export :*sdl-gfx-binaries-user-path*)
  (:export :load-sdl-gfx-library)
  (:export :unload-sdl-gfx-library))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *SDL-GFX-LOADED* nil)

; CHANGE *sdl-gfx-binaries-user-path* TO LOCATE YOUR SDL_GFX DLL
; Justin TODO Frank had some ideas on how to make this more flexible

; First priority, try load the binary from the user-specified location.
(defparameter *sdl-gfx-binaries-user-path* #P"C:/SDL_gfx-2.0.13/lib/SDL_gfx")

; Second priority, try load the binary from the sdl-gfx-binaries package-specified location.
(defparameter *sdl-gfx-binaries-default-path* #P"")

; Third priority, try search all directories in the ASDF:*central-registry*
; for the binary and load the first one found.
(defparameter *sdl-gfx-binaries-asdf-path* #P"SDL_gfx")

; This parameter stores the location from where the binary was loaded,
; in case the user wants to know.
(defparameter *sdl-gfx-binaries-load-path* nil)

; sdl library and sdl init helpers
(defun load-sdl-gfx-library()
  "load the sdl library"
  (if *SDL-GFX-LOADED*
      (format t "SDL_gfx runtime already loaded~%")
      (progn
	(setf *sdl-gfx-binaries-load-path* nil)
	;; Search priority 1
	(if (probe-file (concatenate 'string (namestring *sdl-gfx-binaries-user-path*) ".dll"))
	    (setf *sdl-gfx-binaries-load-path* *sdl-gfx-binaries-user-path*))
	;; Search priority 2
	(unless *sdl-gfx-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-gfx-binaries-default-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-gfx-binaries-load-path* (merge-pathnames *sdl-gfx-binaries-default-path* (eval path))))))
	;; Search priority 3
	(unless *sdl-gfx-binaries-load-path*
	  (dolist (path asdf:*central-registry*)
	    (if (probe-file (merge-pathnames (concatenate 'string
							  (namestring *sdl-gfx-binaries-asdf-path*)
							  ".dll")
					     (eval path)))
		(setf *sdl-gfx-binaries-load-path* (merge-pathnames *sdl-gfx-binaries-asdf-path* (eval path))))))
	;;Attempt to load binary.
	(if *sdl-gfx-binaries-load-path*
	    (format t "Found \"~A\".... " (concatenate 'string (namestring *sdl-gfx-binaries-load-path*) ".dll"))
	    (setf *sdl-gfx-binaries-load-path* #P"SDL_gfx"))
	(format t "attempting to load SDL_gfx runtime.~%")
	(cffi:load-foreign-library *sdl-gfx-binaries-load-path*)
	(setf *SDL-GFX-LOADED* t)
	(format t "Runtime loaded.~%"))))

(defun unload-sdl-gfx-library()
  "Unload the library when done"
  (if *SDL-GFX-LOADED*
      (progn 
	(cffi::close-foreign-library *sdl-gfx-binaries-load-path*)
	(format t "Closed SDL_gfx runtime library~%")
	(setf *SDL-GFX-LOADED* nil))
      (format t "SDL_gfx runtime library is not loaded~%")))
)