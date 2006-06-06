;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The main package of `lispbuilder-sdl-image'.")
  (:export

   ;; util-sdl_image.lisp
   
   ;; sdl_image.lisp
   #:Linked-Version
   #:Load
   #:Load-BMP-RW
   #:Load-GIF-RW
   #:Load-JPG-RW
   #:Load-LBM-RW
   #:Load-PCX-RW
   #:Load-PNG-RW
   #:Load-PNM-RW
   #:Load-RW
   #:Load-TGA-RW
   #:Load-TIF-RW
   #:Load-Typed-RW
   #:Load-XCF-RW
   #:Load-XPM-RW
   #:Load-XV-RW
   #:MAJOR-VERSION
   #:MINOR-VERSION
   #:PATCHLEVEL
   #:Read-XPM-FromArray
   #:VERSION
   #:isBMP
   #:isGIF
   #:isJPG
   #:isLBM
   #:isPCX
   #:isPNG
   #:isPNM
   #:isTIF
   #:isXCF
   #:isXPM
   #:isXV

   ))
