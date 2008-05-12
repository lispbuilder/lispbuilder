;;; -*- lisp -*-

(in-package #:cl-user)

;; sdl-image-util.lisp
;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   (mapcar #'(lambda (symbol)
;; 	      (intern symbol 'lispbuilder-sdl)
;; 	      (export (find-symbol symbol 'lispbuilder-sdl) 'lispbuilder-sdl))
;; 	  (list "IMAGE-P"
;; 		"IMAGE-TYPE-OF"
;; 		"LOAD-IMAGE"
;; 		"LOAD-AND-CONVERT-IMAGE")))
	  
(defpackage #:lispbuilder-sdl-image
  (:use #:cl #:cffi)
  (:nicknames #:sdl-image)
  (:documentation "The methods defined here extend any methods already defined in `lispbuilder-sdl'.")
  ;; (:import-from #:lispbuilder-sdl
;; 		lispbuilder-sdl:image-p
;; 		lispbuilder-sdl:image-type-of
;; 		lispbuilder-sdl:load-and-convert-image
;; 		lispbuilder-sdl:load-image)
  (:export
   ;; sdl-image-util.lisp
   #:image-p
   #:image-type-of
   #:load-image
   #:load-and-convert-image))
