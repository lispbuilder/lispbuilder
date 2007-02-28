;;;; lispbuilder-sdl

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lispbuilder-sdl'.")
  (:import-from #:lispbuilder-sdl-base
		lispbuilder-sdl-base::frame-rate
		lispbuilder-sdl-base::set-frame-rate
		lispbuilder-sdl-base::with-events
		lispbuilder-sdl-base::push-quit-event
		lispbuilder-sdl-base::key=
		lispbuilder-sdl-base::display-cursor
		lispbuilder-sdl-base::is-valid-ptr)
  (:import-from #:lispbuilder-sdl-cffi
		sdl-cffi::sdl-opengl
		lispbuilder-sdl-base::sdl-get-ticks
		lispbuilder-sdl-base::sdl-gl-swap-buffers)
  (:export

   ;; globals.lisp
   #:*default-surface*
   #:*default-display*
   #:*default-color*
   #:*default-position*
   #:*default-rectangle*
   #:*default-font*
   #:*default-font-path*

   #:*default-surface*
   #:default-surface
   #:*default-color*
   #:default-color
   #:*default-position*
   #:default-position
   #:*default-rectangle*
   #:default-rectangle

   #:*sdl-initialized*

   #:*external-init-on-startup*
   #:*external-quit-on-exit*

   ;; init.lisp
   #:with-init
   #:initialize-on-startup
   #:quit-on-exit
   #:list-sub-systems
   #:return-sub-systems-of-status
   #:init-sub-systems
   #:quit-sub-systems
   #:sdl-quit-on-exit
   #:initialized-sub-systems-p
   #:init-sdl
   #:quit-sdl
   #:sdl-init-on-startup

   ;;generics.lisp
   #:fp
   #:fp-position
   #:r
   #:g
   #:b
   #:a
   #:map-color
   #:width
   #:height
   #:x
   #:y
   #:x2
   #:y2
   #:cached-surface
   #:free-font
   #:color=
   #:any-color-but-this

   #:pack-color
   #:color-*
   #:set-color
   #:set-color-*

   #:point-*
   #:get-point
   #:set-point
   #:set-point-*
   #:position-*
   #:get-position
   #:set-position
   #:set-position-*

   #:rectangle-*
   #:get-rectangle
   #:set-rectangle
   #:set-rectangle-*

   #:set-surface
   #:set-surface-*

   #:free-color
   #:free-surface
   #:free-rectangle
   #:free-rwops
   #:free-cached-surface

   #:draw-font
   #:draw-font-at
   #:draw-font-at-*

   #:free-font

   ;; color.lisp
   #:*white*
   #:*black*
   #:sdl-color
   #:color
   #:color-a
   #:with-color
   #:with-foreign-color-copy

   ;; drawing-primitives
   #:add-vertex
   #:add-vertex-*
   #:random-rectangle
   #:rectangle-from-edges-*
   #:rectangle-from-edges
   #:rectangle-from-midpoint-*
   #:genbez
   #:with-bezier
   #:with-curve
   #:with-shape
   #:calculate-curve
   #:catmull-rom-spline
   #:draw-bezier
   #:draw-curve
   #:draw-shape
   #:draw-line-*
   #:draw-line
   #:draw-vline
   #:draw-hline
   #:draw-box
   #:draw-box-*

   #:draw-rectangle
   #:draw-rectangle-*
  
   #:draw-point
   #:draw-point-*
   #:read-point

   #:draw-circle
   #:draw-circle-*
   #:draw-filled-circle
   #:draw-filled-circle-*
   
   ;; events.lisp
   #:with-events

   ;; image.lisp
   #:load-image
   #:save-image

   ;; point.lisp
   #:with-point
   #:point
   #:copy-point

   ;; rectangle.lisp
   #:rectangle
   #:with-rectangle
   #:with-rectangles

   ;; sdl-util.lisp
   #:within-range
   #:within-range-*
   #:distance
   #:distance-*
   #:rotate-surface
   #:flood-fill
   #:flood-fill-*
   #:flood-fill-stack
   #:flood-fill-stack-*
   
   ;; surfaces.lisp
   #:sdl-surface
   #:surface
   #:with-surface
   #:with-surface-slots
   #:with-surfaces
   #:with-locked-surface
   #:with-locked-surfaces
   #:clear-color-key
   #:set-color-key
   #:set-alpha
   #:get-clip-rect
   #:set-clip-rect
   #:get-surface-rect
   #:convert-surface
   #:copy-surface
   #:create-surface
   #:update-surface
   #:update-surface-*
   #:blit-surface
   #:draw-surface
   #:draw-surface-at-*
   #:draw-surface-at
   #:fill-surface
   #:fill-surface-*
   #:fp-cell
   #:clear-cell
   #:set-cell
   #:set-cell-*

   ;; util.lisp
   #:random+1
   #:to-radian
   #:to-degree
   #:cast
   #:cast-to-int
   #:cast-all-to-int
   #:all-integers?
   #:distance
   #:create-list-if-not
   #:check-types

   ;; video.lisp
   #:window
   #:update-display
   #:clear-display

   ;; rwops.lisp
   #:rwops
   #:create-RWops-from-file

   ;; bitmap-font.lisp
;;    #:font
;;    #:load-font
   #:char-width
   #:char-height
   #:with-default-font
   #:with-font
;;    #:with-load-font   
   #:initialise-default-font
   #:initialise-font
   
   ;; bitmap-font-data.lisp
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

   ;; string-solid.lisp
   #:render-string-solid
   #:draw-string-solid
   #:draw-string-solid-*

   ;; string-shaded.lisp
   #:render-string-shaded
   #:draw-string-shaded
   #:draw-string-shaded-*

   
   ;; Imports from lispbuilder-sdl-cffi
   #:sdl-get-ticks
   #:sdl-opengl
   #:sdl-gl-swap-buffers
   
   ;; Imports from lispbuilder-sdl-base
   #:frame-rate
   #:set-frame-rate
   #:with-events
   #:push-quit-event
   #:key=
   #:display-cursor
   #:is-valid-ptr
   ))


