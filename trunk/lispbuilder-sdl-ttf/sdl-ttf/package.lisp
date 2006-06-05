;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:export

   ;; util-sdl_ttf.lisp

   #:close-font
   #:get-Glyph-Metric
   #:get-Size-Text
   #:get-Size-UTF8
   #:make-text-surface
   #:open-font
   #:with-init
   
   ;; sdl_ttf.lisp

   #:Byte-Swapped-UNICODE
   #:Close-Font
   #:Get-Font-Style
   #:Init
   #:Linked-Version
   #:MAJOR-VERSION
   #:MINOR-VERSION
   #:Open-Font
   #:Open-Font-Index
   #:Open-Font-Index-RW
   #:Open-Font-RW
   #:PATCHLEVEL
   #:Quit
   #:Render-Glyph-Blended
   #:Render-Glyph-Shaded
   #:Render-Glyph-Solid
   #:Render-Text-Blended
   #:Render-Text-Shaded
   #:Render-Text-Solid
   #:Render-UTF8-Blended
   #:Render-UTF8-Shaded
   #:Render-UTF8-Solid
   #:Set-Font-Style
   #:VERSION
   #:Was-Init
   #:get-Font-Ascent
   #:get-Font-Descent
   #:get-Font-Face-Family-Name
   #:get-Font-Face-Is-Fixed-Width
   #:get-Font-Face-Style-Name
   #:get-Font-Faces
   #:get-Font-Height
   #:get-Font-Line-Skip
   ;; #:Render-UNICODE-Solid
   ;; #:RenderUNICODE-Blended
   ;; #:RenderUNICODE-Shaded
   ;; #:get-Size-UNICODE
   ))
