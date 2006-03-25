;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx
  (:nicknames :sdl-gfx)
  (:use #:cl #:cffi)
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
	   ;;
	   #:*sdl-gfx-binaries-load-path* #:*sdl-gfx-binaries-user-path* #:load-sdl-gfx-library
	   #:unload-sdl-gfx-library))

(in-package #:lispbuilder-sdl-gfx) 

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *SDL-GFX-LOADED* nil)

; CHANGE *sdl-gfx-binaries-user-path* TO LOCATE YOUR SDL_GFX DLL
; Justin TODO Frank had some ideas on how to make this more flexible

  ;; First priority, try load the binary from the user-specified location.
  (defparameter *sdl-gfx-binaries-user-path* #P"C:/SDL_gfx-2.0.13/lib/SDL_gfx")

  ;; Second priority, try load the binary from the sdl-gfx-binaries package-specified location.
  (defparameter *sdl-gfx-binaries-default-path* #P"")
  
  ;; Third priority, try search all directories in the ASDF:*central-registry*
  ;; for the binary and load the first one found.
  (defparameter *sdl-gfx-binaries-asdf-path* #P"SDL_gfx")
  
  ;; This parameter stores the location from where the binary was loaded,
  ;; in case the user wants to know.
  (defparameter *sdl-gfx-binaries-load-path* nil)
  
  ;; sdl library and sdl init helpers
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
  
  #+(and :win32 :clisp) (load-sdl-gfx-library)
  #+(and unix cmu) (ext:load-foreign "/usr/lib/libSDL_gfx.so")
  )

(defun unload-sdl-gfx-library()
  "Unload the library when done"
  (if *SDL-GFX-LOADED*
      (progn 
	(cffi::close-foreign-library *sdl-gfx-binaries-load-path*)
	(format t "Closed SDL_gfx runtime library~%")
	(setf *SDL-GFX-LOADED* nil))
      (format t "SDL_gfx runtime library is not loaded~%")))
