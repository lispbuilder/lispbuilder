;;;; lispbuilder-sdl

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lispbuilder-sdl'.")

  (:import-from #:lispbuilder-sdl-base
   lispbuilder-sdl-base::is-valid-ptr
   lispbuilder-sdl-base::*default-fpsmanager*              
   lispbuilder-sdl-base::1/0->t/nil
   lispbuilder-sdl-base::fps-fixed
   lispbuilder-sdl-base::fps-unlocked)

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
   lispbuilder-sdl-cffi::SDL-WM-GRAB-INPUT)

  (:import-from #:lispbuilder-sdl-assets
   lispbuilder-sdl-assets::*default-asset-path*)

  (:export

   ;; globals.lisp
   #:*default-surface*
   #:*default-display*
   #:*default-color*
   #:*default-position*
   #:*default-rectangle*
   #:*default-font*

   #:*default-font-path*
   #:*default-audio-path*
   #:*default-image-path*
   
   #:*default-surface*
   #:*default-color*
   #:*default-position*
   #:*default-rectangle*

   #:*sdl-initialized*

   #:*external-init-subsystems-on-startup*
   #:*external-quit-subsystems-on-exit*

   #:*opengl-context*

   ;;#:*default-font-path*
   #:*default-simple-font*
   #:*default-ttf-font*
   
   #:*mixer*
   #:*managed-audio*
   #:+DEFAULT-FORMAT+
   #:+CHANNELS+
   #:+DEFAULT-FREQUENCY+
   #:+DEFAULT-CHANNELS+
   #:+MAX-VOLUME+
   #:+DEFAULT-SAMPLE-BUFFER+
   #:+CHANNEL-POST+
   #:+MAX-AUDIO-16+
   #:+MIN-AUDIO-16+
   #:+MAX-AUDIO-8+
   #:+MIN-AUDIO-8+
   
   ;; init.lisp
   #:with-init
   #:initialize-subsystems-on-startup
   #:quit-subsystems-on-exit
   #:init-on-startup
   #:quit-on-exit
   #:list-subsystems
   #:return-subsystems-of-status
   #:init-subsystems
   #:quit-subsystems
   #:initialized-subsystems-p
   #:init-sdl
   #:quit-sdl

   ;; input-util.lisp
   #:initialise-input-util
   #:update-input-util
   #:quit-input-util
   #:key-pressed-p 
   #:key-released-p 
   #:key-held-p
   #:key-time-in-current-state
   #:key-time-in-previous-state
   #:handle-key-up
   #:handle-key-down

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
   #:new-event
   #:push-quit-event
   #:push-user-event

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
   #:enable-alpha
   #:alpha-enabled-p
   #:alpha
   #:color-key-enabled-p
   #:enable-color-key
   #:color-key
   #:pixel-alpha-enabled-p
   #:rle-accel-enabled-p
   #:enable-rle-accel
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
   #:set-caption
   #:render-state
   
   ;; rwops.lisp
   #:rwops
   #:create-RWops-from-file

   ;; bitmap-font.lisp
   ;;    #:font
   ;;    #:load-font
   #:bitmap-font
   #:sdl-bitmap-font
   #:gfx-bitmap-font
   #:char-width
   #:char-height
   #:set-default-font
   #:default-font-p

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

   ;; simple-font-data.lisp
   #:*simple-font-4x5*

   ;; simple-font.lisp
   #:simple-font-definition
   #:create-simple-font-definition
   
   ;; string-solid.lisp
   #:render-string-solid
   #:draw-string-solid
   #:draw-string-solid-*

   ;; string-shaded.lisp
   #:render-string-shaded
   #:draw-string-shaded
   #:draw-string-shaded-*

   ;; string-blended.lisp
   #:render-string-blended
   #:draw-string-blended
   #:draw-string-blended-*
   
   ;; pixel.lisp
   #:bit-depth

   ;; audio.lisp
   #:audio-spec
   #:print-object
   #:sample-frequency
   #:audio-format
   #:output-channels
   #:audio-silence
   #:audio-buffer-size
   #:audio-buffer-size-calculated
   #:spec-callback
   #:spec-user-data
   #:audio-cvt
   #:audio-buffer
   #:audio
   #:audio-buffer-handle
   #:audio-remaining
   #:audio-position
   #:audio-paused-p
   #:volume
   #:callback-finished
   #:copy-audio
   #:audio-length
   #:register-audio-finished
   #:unregister-audio-finished
   #:audio-finished-callback
   #:resume-audio
   #:halt-sample
   #:rewind-audio
   #:audio-playing-p
   #:audio-playable-p
   #:audio-opened-p
   #:mixer
   #:requested-sample-frequency
   #:requested-audio-format
   #:requested-output-channels
   #:requested-audio-buffer-size
   #:audio-spec
   #:mixer-opened-p
   #:output-audio-buffer-size
   #:output-buffer
   #:audio-volume
   #:open-audio
   #:play-audio
   #:pause-audio
   #:close-audio
   #:load-sample
   #:build-audio-cvt
   #:load-audio
   #:default-fill-audio-buffer
   
   ;; assets/globals.lisp
   ;;#:*default-font-path*

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
   #:frame-time
   #:time-scale
   #:register-physics

   ;; keys.lisp
   #:enable-unicode-p
   #:enable-unicode
   #:enable-key-repeat
   #:disable-key-repeat
   #:enable-key-repeat-p
   #:key-repeat-delay
   #:key-repeat-interval
   #:get-key-state
   #:key=
   #:modifier=
   #:modifier-p
   #:modifier-in

   ;; font.lisp
   #:with-default-font
   #:with-font
   #:set-font-style
   #:get-Glyph-Metric
   #:get-Font-Size
   #:get-font-style
   #:get-font-height
   #:get-font-ascent
   #:get-font-descent
   #:get-font-line-skip
   #:get-font-faces
   #:is-font-face-fixed-width
   #:get-font-face-family-name
   #:get-font-face-style-name

   ;; Imports from lispbuilder-sdl-base  
   #:*default-fpsmanager*
   #:with-events
   #:push-quit-event
   #:key=
   #:average-fps

   #:is-valid-ptr
   #:push-user-event
   #:fps-fixed
   #:fps-timestep

   ;; Imports from lispbuilder-sdl-assets
   #:*default-asset-path*))


