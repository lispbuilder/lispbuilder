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
		lispbuilder-sdl-base::is-valid-ptr
		lispbuilder-sdl-base::push-user-event)
  (:import-from #:lispbuilder-sdl-cffi
		sdl-cffi::sdl-opengl
		lispbuilder-sdl-base::sdl-get-ticks

		lispbuilder-sdl-cffi::sdl-init-everything
		lispbuilder-sdl-cffi::sdl-init-video
		lispbuilder-sdl-cffi::sdl-init-cdrom
		lispbuilder-sdl-cffi::sdl-init-audio 
		lispbuilder-sdl-cffi::sdl-init-timer
		lispbuilder-sdl-cffi::sdl-init-joystick
		lispbuilder-sdl-cffi::sdl-init-eventthread
		lispbuilder-sdl-cffi::sdl-init-noparachute

		lispbuilder-sdl-cffi::sdl-sw-surface
		lispbuilder-sdl-cffi::sdl-hw-surface
		lispbuilder-sdl-cffi::sdl-async-blit
		lispbuilder-sdl-cffi::sdl-any-format
		lispbuilder-sdl-cffi::sdl-hw-palette
		lispbuilder-sdl-cffi::sdl-doublebuf
		lispbuilder-sdl-cffi::sdl-fullscreen
		lispbuilder-sdl-cffi::sdl-opengl
		lispbuilder-sdl-cffi::sdl-opengl-blit
		lispbuilder-sdl-cffi::sdl-resizable
		lispbuilder-sdl-cffi::sdl-no-frame
		lispbuilder-sdl-cffi::sdl-hw-accel
		lispbuilder-sdl-cffi::sdl-src-color-key
		lispbuilder-sdl-cffi::sdl-rle-accel-ok
		lispbuilder-sdl-cffi::sdl-rle-accel
		lispbuilder-sdl-cffi::sdl-src-alpha
		lispbuilder-sdl-cffi::sdl-pre-alloc
		lispbuilder-sdl-cffi::sdl-yv12-overlay
		lispbuilder-sdl-cffi::sdl-iyuv-overlay
		lispbuilder-sdl-cffi::sdl-yuy2-overlay
		lispbuilder-sdl-cffi::sdl-uyvy-overlay
		lispbuilder-sdl-cffi::sdl-yvyu-overlay
		)
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
   #:*default-color*
   #:*default-position*
   #:*default-rectangle*

   #:*sdl-initialized*

   #:*external-init-on-startup*
   #:*external-quit-on-exit*

   #:*opengl-context*

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

   ;; default-colors.lisp
   #:*black*
   #:*white*
   #:*red*
   #:*green*
   #:*blue*
   #:*yellow*
   #:*cyan*
   #:*magenta*
   
   ;; color.lisp
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

   #:draw-trigon
   #:draw-polygon
   
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
   #:create-path

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

   ;; pixel.lisp
   #:bit-depth
   
   ;; Imports from lispbuilder-sdl-cffi
   #:sdl-get-ticks
   #:sdl-opengl
   #:sdl-init-everything
   #:sdl-init-video
   #:sdl-init-cdrom
   #:sdl-init-audio 
   #:sdl-init-timer
   #:sdl-init-joystick
   #:sdl-init-eventthread
   #:sdl-init-noparachute
   
   #:sdl-sw-surface
   #:sdl-hw-surface
   #:sdl-async-blit
   #:sdl-any-format
   #:sdl-hw-palette
   #:sdl-doublebuf
   #:sdl-fullscreen
   #:sdl-opengl
   #:sdl-opengl-blit
   #:sdl-resizable
   #:sdl-no-frame

   #:sdl-hw-accel
   #:sdl-src-color-key
   #:sdl-rle-accel-ok
   #:sdl-rle-accel
   #:sdl-src-alpha
   #:sdl-pre-alloc
   #:sdl-yv12-overlay
   #:sdl-iyuv-overlay
   #:sdl-yuy2-overlay
   #:sdl-uyvy-overlay
   #:sdl-yvyu-overlay

   
   ;; Imports from lispbuilder-sdl-base
   #:frame-rate
   #:set-frame-rate
   #:with-events
   #:push-quit-event
   #:key=
   #:display-cursor
   #:is-valid-ptr
   #:push-user-event
   ))


