;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-ttf
  (:use #:cl #:cffi)
  (:nicknames #:sdl-ttf)
  (:documentation "The main package of `lispbuilder-sdl-ttf'.")
  (:export

   ;; util-sdl_ttf.lisp
   #:render-font-blended
   #:render-font-shaded
   #:render-font-solid
   #:get-Glyph-Metric
   #:get-Font-Size
   #:open-font
   #:with-init
   #:with-open-font
   *default-font*
   
   ;; sdl_ttf.lisp

 ;  #:Byte-Swapped-UNICODE
   #:Close-Font
   #:Get-Font-Style
;   #:Init
   #:Linked-Version
   #:MAJOR-VERSION
   #:MINOR-VERSION
   #:Open-Font
   #:Open-Font-Index
;   #:Open-Font-Index-RW
;   #:Open-Font-RW
   #:PATCHLEVEL
   #:Quit
   #:Set-Font-Style
   #:VERSION
   #:is-Init
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
