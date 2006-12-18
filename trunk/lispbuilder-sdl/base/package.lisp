;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-base
  (:use #:cl #:cffi)
  (:nicknames #:sdl-base #:sbl)
  (:documentation "The basic wrapper package of `lispbuilder-sdl'.")
  (:export

   ;; util.lisp
   #:check-bounds
   #:clamp
   #:is-valid-ptr
   #:to-int

   
   ;; events.lisp
   #:new-event
   #:push-quitevent
   #:get-timescale
   #:set-timescale
   #:set-framerate
   #:get-framerate
   #:with-events

   ;; rectangle.lisp
   #:with-rectangle
   #:rect-x
   #:rect-y
   #:rect-w
   #:rect-h
   #:rect-x2
   #:rect-y2
   #:copy-rectangle
   #:clone-rectangle
   #:rectangle

   ;; rwops.lisp
   #:create-RWops-from-file

   ;; surfaces.lisp
   #:with-locked-surface
   #:with-possible-lock-and-update
   #:with-surface
   #:with-surfaces
   #:with-surface-free
   #:clear-colorkey
   #:set-colorkey
   #:get-surface-rect
   #:convert-surface-to-display-format
   #:copy-surface
   #:create-surface
   #:must-lock?
   #:pixel-format
   #:set-alpha
   #:surf-w
   #:surf-h
   #:update-surface
   #:blit-surface
   #:fill-surface


   ;; video.lisp
   #:with-display
   #:display-cursor
   #:get-native-window
   #:get-video-info
   #:list-modes
   #:query-cursor
   #:set-screen
   #:set-window
   #:update-display
   #:clear-display
   #:video-driver-name))