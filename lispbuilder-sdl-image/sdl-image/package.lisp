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
  (:import-from #:lispbuilder-sdl
		#:init-image
		#:image-init-p
		#:quit-image
		#:image-p
		#:image-type-of
		#:load-image
		#:load-and-convert-image
		#:image-library-version)
  (:export
   #:init-image
   #:image-init-p
   #:quit-image
   #:image-p
   #:image-type-of
   #:load-image
   #:load-and-convert-image
   #:image-library-version

   ;; sdl-image-util.lisp
   #:load-library))
