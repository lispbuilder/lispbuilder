;;;; lispbuilder-sdl

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lispbuilder-sdl'.")
  (:import-from #:lispbuilder-sdl-base
		lispbuilder-sdl-base::with-events
		lispbuilder-sdl-base::push-quit-event
		lispbuilder-sdl-base::key=

		lispbuilder-sdl-base::is-valid-ptr
		lispbuilder-sdl-base::push-user-event
		
		lispbuilder-sdl-base::*default-fpsmanager*
		lispbuilder-sdl-base::*fps-average*

		lispbuilder-sdl-base::1/0->t/nil)
  (:import-from #:lispbuilder-sdl-cffi
		lispbuilder-sdl-cffi::sdl-get-ticks

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

		lispbuilder-sdl-cffi::Num-Joysticks
		lispbuilder-sdl-cffi::SDL-Joystick-Name
		lispbuilder-sdl-cffi::SDL-WM-GRAB-INPUT
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
   #:map-color-*
   #:width
   #:height
   #:x
   #:y
   #:x2
   #:y2
   #:cached-surface
   #:free
   #:color=
   #:any-color-but-this
   #:w
   #:h

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

   #:free-cached-surface

   #:draw-font
   #:draw-font-at
   #:draw-font-at-*

   #:image-p
   #:image-type-of

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
  
   #:draw-pixel
   #:draw-pixel-*
   #:read-pixel
   #:read-pixel-*

   #:draw-circle
   #:draw-circle-*
   #:draw-filled-circle
   #:draw-filled-circle-*

   #:draw-trigon
   #:draw-filled-trigon
   
   #:draw-polygon
   #:draw-filled-polygon

   #:draw-ellipse
   #:draw-ellipse-*
   #:draw-filled-ellipse
   #:draw-filled-ellipse-*

   #:draw-pie
   #:draw-pie-*
   #:draw-filled-pie
   #:draw-filled-pie-*
  
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
   #:rectangle-from-edges-*
   #:rectangle-from-edges
   #:rectangle-from-midpoint-*
   #:with-rectangle
   #:with-rectangles

   ;; sdl-util.lisp
   #:within-range
   #:within-range-*
   #:distance
   #:distance-*
   #:rotate-surface
   #:rotate-surface-xy
   #:zoom-surface
   #:flood-fill
   #:flood-fill-*
   #:flood-fill-stack
   #:flood-fill-stack-*
   
   ;; surfaces.lisp
   #:sdl-surface
   #:surface
   #:display-surface
   #:with-surface
   #:with-surface-slots
   #:with-surfaces
   #:with-locked-surface
   #:with-locked-surfaces
   #:clear-color-key
   #:alpha-enabled?
   #:alpha
   #:color-key-enabled?
   #:color-key
   #:pixel-alpha-enabled?
   #:rle-accel-enabled?
   #:clip-rect
   #:clear-clip-rect
   #:get-clip-rect
   #:set-clip-rect
   #:get-surface-rect
   #:convert-surface
   #:convert-to-display-format
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
   #:copy-channel-to-alpha

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

   #:display-cursor
   #:video-driver-name
   #:query-cursor
   #:show-cursor
   #:video-info
   #:surface-info
   #:get-native-window
   #:list-modes
   #:video-memory
   #:video-dimensions
   #:set-gl-attribute
   
   ;; rwops.lisp
   #:rwops
   #:create-RWops-from-file

   ;; bitmap-font.lisp
;;    #:font
;;    #:load-font
   #:bitmap-font
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

   #:Num-Joysticks
   #:SDL-Joystick-Name

   #:SDL-WM-GRAB-INPUT
   
   ;; fps.lisp
   #:frame-rate
   #:average-fps
   #:time-scale

   ;; keys.lisp
   #:enable-unicode-p
   #:enable-unicode
   #:enable-key-repeat
   #:disable-key-repeat
   #:enable-key-repeat-p
   #:key-repeat-delay
   #:key-repeat-interval
   #:get-key-state
   
   ;; Imports from lispbuilder-sdl-base  
   #:with-events
   #:push-quit-event
   #:key=
   #:average-fps

   #:is-valid-ptr
   #:push-user-event
   ))


