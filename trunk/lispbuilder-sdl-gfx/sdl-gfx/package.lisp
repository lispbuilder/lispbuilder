;;; -*- lisp -*-
 
(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-gfx
  (:use #:cl #:cffi)
  (:nicknames #:sdl-gfx)
  (:documentation "The main package of `lispbuilder-gfx-sdl'.")
  (:import-from #:lispbuilder-sdl
		lispbuilder-sdl:draw-font
		lispbuilder-sdl:draw-font-at
		lispbuilder-sdl:draw-font-at-*
		lispbuilder-sdl:*font-5x7*
		lispbuilder-sdl:*font-5x8*
		lispbuilder-sdl:*font-6x9*
		lispbuilder-sdl:*font-6x10*
		lispbuilder-sdl:*font-6x12*
		lispbuilder-sdl:*font-6x13*
		lispbuilder-sdl:*font-6x13B*
		lispbuilder-sdl:*font-6x13O*
		lispbuilder-sdl:*font-7x13*
		lispbuilder-sdl:*font-7x13B*
		lispbuilder-sdl:*font-7x13O*
		lispbuilder-sdl:*font-7x14*
		lispbuilder-sdl:*font-7x14B*
		lispbuilder-sdl:*font-8x8*
		lispbuilder-sdl:*font-8x13*
		lispbuilder-sdl:*font-8x13B*
		lispbuilder-sdl:*font-8x13O*
		lispbuilder-sdl:*font-9x15*
		lispbuilder-sdl:*font-9x15B*
		lispbuilder-sdl:*font-9x18*
		lispbuilder-sdl:*font-9x18B*
		lispbuilder-sdl:*font-10x20*)
  (:export

   ;;; globals.lisp
   #:*default-font*

   ;;; gfx.lisp
   
;;    #:FPS-DEFAULT
;;    #:FPS-LOWER-LIMIT
;;    #:FPS-UPPER-LIMIT
   #:GFX-FONT-DATA-MAX
   #:M-PI
   #:SDL-GFX-PRIMITIVES-MAJOR
   #:sdl-GFX-PRIMITIVES-MICRO
   #:sdl-GFX-PRIMITIVES-MINOR
;;    #:SDL-frame-rate-Delay
;;    #:SDL-getFramerate
;;    #:SDL-image-Filter-Abs-Diff
;;    #:SDL-image-Filter-Add
;;    #:SDL-image-Filter-Add-Byte
;;    #:sdl-image-Filter-Add-Byte-To-Half
;;    #:sdl-image-Filter-Add-Uint
;;    #:sdl-image-Filter-Align-Stack
;;    #:sdl-image-Filter-Binarize-Using-Threshold
;;    #:sdl-image-Filter-Bit-And
;;    #:sdl-image-Filter-Bit-Negation
;;    #:sdl-image-Filter-Bit-Or
;;    #:sdl-image-Filter-Clip-To-Range
;;    #:sdl-image-Filter-Convolve-Kernel-3x3-Divide
;;    #:sdl-image-Filter-Convolve-Kernel-3x3-Shift-Right
;;    #:sdl-image-Filter-Convolve-Kernel-5x5-Divide
;;    #:sdl-image-Filter-Convolve-Kernel-5x5-Shift-Right
;;    #:sdl-image-Filter-Convolve-Kernel-7x7-Divide
;;    #:sdl-image-Filter-Convolve-Kernel-7x7-Shift-Right
;;    #:sdl-image-Filter-Convolve-Kernel-9x9-Divide
;;    #:sdl-image-filter-Convolve-Kernel-9x9-Shift-Right
;;    #:sdl-image-filter-Div
;;    #:sdl-image-filter-MMX-detect
;;    #:sdl-image-filter-MMX-off
;;    #:sdl-image-filter-MMX-on
;;    #:sdl-image-filter-Mean
;;    #:sdl-image-filter-Mult
;;    #:sdl-image-filter-Mult-By-Byte
;;    #:sdl-image-filter-Mult-Div-by-2
;;    #:sdl-image-filter-Mult-Div-by-4
;;    #:sdl-image-filter-Mult-Nor
;;    #:sdl-image-filter-Normalize-Linear
;;    #:sdl-image-filter-Restore-Stack
;;    #:sdl-image-filter-Shift-Left
;;    #:sdl-image-filter-Shift-Left-Byte
;;    #:sdl-image-filter-Shift-Left-Uint
;;    #:sdl-image-filter-Shift-Right
;;    #:sdl-image-filter-Shift-Right-And-Mult-By-Byte
;;    #:sdl-image-filter-Shift-Right-Uint
;;    #:sdl-image-filter-Sobel-X
;;    #:sdl-image-filter-Sobel-X-Shift-Right
;;    #:sdl-image-filter-Sub
;;    #:sdl-image-filter-Sub-Byte
;;    #:sdl-image-filter-Sub-Uint
;;    #:sdl-init-Frame-rate
;;    #:sdl-set-Frame-rate
   #:SMOOTHING-OFF
   #:SMOOTHING-ON

   ;;; sdl-gfx-util.lisp   
   #:add-vertex
   #:add-vertex-*
   #:draw-aa-circle
   #:draw-aa-circle-*
   #:draw-aa-ellipse
   #:draw-aa-ellipse-*
   #:draw-aa-line
   #:draw-aa-line-*
   #:draw-aa-polygon
   #:draw-aa-trigon
   #:draw-bezier
   #:draw-box
   #:draw-box-*
   #:draw-circle
   #:draw-circle-*
   #:draw-curve
   #:draw-ellipse
   #:draw-ellipse-*
   #:draw-filled-Circle
   #:draw-filled-Circle-*
   #:draw-filled-Ellipse
   #:draw-filled-Ellipse-*
   #:draw-filled-Pie
   #:draw-filled-Pie-*
   #:draw-filled-Polygon
   #:draw-filled-Trigon
   #:draw-hline
   #:draw-line
   #:draw-line-*
   #:draw-pie
   #:draw-pie-*
   #:draw-pixel
   #:draw-pixel-*
   #:draw-polygon
   #:draw-rectangle
   #:draw-rectangle-*
   #:roto-zoom-surface
   #:roto-zoom-Size
   #:roto-zoom-Size-XY
   #:roto-zoom-XY
   #:draw-shape
   #:draw-trigon
   #:draw-vline
   #:draw-vline
   #:zoom-surface
   #:zoom-surface-Size
   #:gfx-Primitives-Set-Font
   #:t-Color-RGBA
   #:t-Color-Y
   #:with-bezier
   #:with-curve
   #:with-shape

   #:rotate-surface
   #:rotate-surface-xy
   #:zoom-surface

   ;;; font.lisp
   #:font
   #:free-font
   #:initialise-default-font
   #:initialise-font
   #:render-string-shaded
   #:draw-character-shaded
   #:draw-character-shaded-*
   #:draw-string-shaded
   #:draw-string-shaded-*
   #:render-string-solid
   #:draw-character-solid
   #:draw-character-solid-*
   #:draw-string-solid
   #:draw-string-solid-*
   #:set-default-font
   #:font-width
   #:font-height

   ;;; Exports from lispbuilder-sdl
   #:draw-font
   #:draw-font-at
   #:draw-font-at-*

   ;;; Exports from lispbuilder-sdl-gfx-cffi
   #:*font-8x8*
   #:*font-5x7*
   #:*font-5x8*
   #:*font-6x9*
   #:*font-6x10*
   #:*font-6x12*
   #:*font-6x13*
   #:*font-6x13B*
   #:*font-6x13O*
   #:*font-7x13*
   #:*font-7x13B*
   #:*font-7x13O*
   #:*font-7x14*
   #:*font-7x14B*
   #:*font-8x13*
   #:*font-8x13B*
   #:*font-8x13O*
   #:*font-9x15*
   #:*font-9x15B*
   #:*font-9x18*
   #:*font-9x18B*
   #:*font-10x20*
   ))

