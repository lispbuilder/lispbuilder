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
;;; $Id: vecto.asd,v 1.10 2007/10/01 16:24:50 xach Exp $

(asdf:defsystem #:vecto
  :depends-on (#:cl-vectors
               (:version #:salza-png "1.0.1")
               #:zpb-ttf)
  :version "1.0.2"
  :components ((:file "package")
               (:file "utils"
                      :depends-on ("package"))
               (:file "copy"
                      :depends-on ("package"))
               (:file "color"
                      :depends-on ("package"
                                   "copy"))
               (:file "paths"
                      :depends-on ("package"))
               (:file "transform-matrix"
                      :depends-on ("package"))
               (:file "clipping-paths"
                      :depends-on ("package"
                                   "copy"))
               (:file "graphics-state"
                      :depends-on ("package"
                                   "color"
                                   "clipping-paths"
                                   "transform-matrix"
                                   "copy"))
               (:file "drawing"
                      :depends-on ("package"
                                   "utils"
                                   "paths"
                                   "graphics-state"
                                   "transform-matrix"))
               (:file "text"
                      :depends-on ("package"
                                   "transform-matrix"
                                   "graphics-state"
                                   "drawing"))
               (:file "user-drawing"
                      :depends-on ("package"
                                   "utils"
                                   "clipping-paths"
                                   "graphics-state"
                                   "transform-matrix"
                                   "text"))
               (:file "user-shortcuts"
                      :depends-on ("user-drawing"))))

