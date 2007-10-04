;;; -*- lisp -*-

(defpackage #:lispbuilder-sdl-vecto-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-vecto-system)

(defsystem lispbuilder-sdl-vecto
  :description "VECTO v1.0.2 glue for LISPBUILDER-SDL"
  :version "0.0.1"
  :depends-on (lispbuilder-sdl-cl-vectors)
  :components
  ((:module "vecto"
	    :components
	    ((:file "package")
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
   (:module "glue-vecto"
	    :components
	    ((:file "package")
	     (:file "glue-vecto"
		    :depends-on ("package"))))))

