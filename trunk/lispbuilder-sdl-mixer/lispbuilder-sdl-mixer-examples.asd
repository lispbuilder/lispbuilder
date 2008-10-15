;;;; -*- lisp -*-
;;;; Examples for lispbuilder sdl mixer
;;;; By Justin Heyes-Jones
;;;; Thanks to Luke Crook for package help, testing and feedback

(defpackage #:lispbuilder-sdl-mixer-examples-system
  (:use #:cl #:asdf))
(in-package #:lispbuilder-sdl-mixer-examples-system)

(defsystem lispbuilder-sdl-mixer-examples
    :description "Examples for the lispbuilder-sdl-mixer package."
    :version "0.3"
    :depends-on (cffi lispbuilder-sdl lispbuilder-sdl-mixer)
    :components
    ((:module "examples"
	      :components
	      ((:file "package")
	       (:file "globals" :depends-on ("package"))
	       (:file "mixer" :depends-on ("globals" "package"))
	       (:static-file "music.mp3")
	       (:static-file "music.ogg")
	       (:static-file "phaser.wav")))))
	       


