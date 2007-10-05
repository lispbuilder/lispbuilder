;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; $Id: package.lisp,v 1.17 2007/10/01 14:13:11 xach Exp $

(cl:defpackage #:vecto
  (:use #:cl)
  (:import-from #:zpb-ttf
                #:open-font-loader
                #:xmin
                #:xmax
                #:ymin
                #:ymax
                #:bounding-box)
  (:export
   ;; canvas operations
   #:with-canvas
   #:clear-canvas
   #:save-png
   #:save-png-stream
   ;; path construction
   #:move-to
   #:line-to
   #:curve-to
   #:quadratic-to
   #:close-subpath
   ;; Clipping
   #:end-path-no-op
   #:clip-path
   #:even-odd-clip-path
   ;; path construction one-offs
   #:rectangle
   #:rounded-rectangle
   #:centered-ellipse-path
   #:centered-circle-path
   #:+kappa+
   ;; painting
   #:fill-path
   #:even-odd-fill
   #:stroke
   #:fill-and-stroke
   #:even-odd-fill-and-stroke
   ;; graphics state
   #:with-graphics-state
   #:set-line-cap
   #:set-line-join
   #:set-line-width
   #:set-dash-pattern
   #:set-rgba-stroke
   #:set-rgb-stroke
   #:set-rgba-fill
   #:set-rgb-fill
   ;; graphics state coordinate transforms
   #:translate
   #:rotate
   #:rotate-degrees
   #:skew
   #:scale
   ;; text
   #:get-font
   #:set-font
   #:draw-string
   #:string-bounding-box
   #:draw-centered-string))
