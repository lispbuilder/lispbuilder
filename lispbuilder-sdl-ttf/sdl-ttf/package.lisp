;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:export

   ;; globals.lisp
   #:*default-font*
   #:*default-font-path*
   
   ;; font.lisp
   #:font
   #:new-font
;;    #:x
;;    #:y
;;    #:fp-position
   #:free-font

   ;; sdl-util-ttf.lisp
   #:is-init
;;   #:with-init
   #:with-default-font
   #:with-open-font
   #:init-ttf
   #:quit-ttf
   #:initialise-font
   #:initialise-default-font
   #:close-font
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
   #:open-font
   #:draw-string-solid
   #:draw-string-solid-*
   #:draw-string-shaded
   #:draw-string-shaded-*
   #:draw-string-blended
   #:draw-string-blended-*
   #:draw-font
   #:draw-font-at
   #:draw-font-at-*
   #:set-font-style
   
   
   ))
