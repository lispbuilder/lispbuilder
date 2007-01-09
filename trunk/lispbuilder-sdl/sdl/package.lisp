;;;; lispbuilder-sdl

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl
  (:use #:cl #:cffi)
  (:nicknames #:sdl)
  (:documentation "The main package of `lispbuilder-sdl'.")
  (:export

   ;; globals.lisp
   *default-surface*
   *default-display*
   *default-color*
   *default-position*
   *default-rectangle*
   *default-font*
   *default-font-path*

   *default-surface*
   default-surface
   *default-color*
   default-color
   *default-position*
   default-position
   *default-rectangle*
   default-rectangle

   *sdl-initialized*

   ;; init.lisp
   with-init
   initialize-on-startup
   quit-on-exit
   list-sub-systems
   return-sub-systems-of-status
   init-sub-systems
   quit-sub-systems
   sdl-quit-on-exit
   initialized-sub-systems-p
   init-sdl
   quit-sdl
   sdl-init-on-startup

   ;;generics.lisp
   fp
   fp-position
   r
   g
   b
   a
   map-color
   set-color
   width
   height
   x
   y
   x2
   x2
   point-from
   xy
   set-xy
   pos
   free-color
   free-surface
   free-rectangle
   free-rwops

   ;; color.lisp
   color
   with-color

   ;; drawing-primitives
   add-vertex
   random-rectangle
   rectangle-from-xy
   rectangle-from-points
   rectangle-from-midpoint
   rectangle-from-surface
   genbez
   with-bezier
   with-curve
   with-shape
   calculate-curve
   catmull-rom-spline
   draw-bezier
   draw-curve
   draw-shape
   draw-line-xy
   draw-line
   draw-vline-points
   draw-hline-points
   draw-vline-xy
   draw-hline-xy
   draw-box
   draw-box-points
   draw-box-xy
   draw-rectangle
   draw-rectangle-points
   draw-rectangle-xy
   draw-point
   read-point

   ;; events.lisp
   with-events

   ;; image.lisp
   load-image

   ;; point.lisp
   with-point
   point

   ;; rectangle.lisp
   rectangle
   with-rectangle
   with-rectangles
   set-rectangle

   ;; sdl-util.lisp
   points-in-range
   distance-to-point
   rotate-surface
   font
   initialise-font
   free-font
   make-text-image
   draw-character
   draw-string
   draw-string-left-justify
   draw-string-right-justify
   draw-string-centered
   draw-font
   initialise-default-font
   flood-fill
   flood-fill-stack
   
   
   ;; surfaces.lisp
   surface
   with-surface
   with-surface-slots
   with-surfaces
   with-locked-surface
   with-locked-surfaces
   clear-color-key
   set-color-key
   set-alpha
   get-clip-rect
   set-clip-rect
   get-surface-rect
   convert-surface
   copy-surface
   create-surface
   update-surface
   blit-surface
   draw-surface
   draw-surface-at
   fill-surface

   ;; util.lisp
   random+1
   to-radian
   to-degree
   distance
   create-list-if-not

   ;; video.lisp
   window
   update-display
   clear-display

   ;; rwops.lisp
   rwops
   create-RWops-from-file
))
