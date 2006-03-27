;;;; Demonstration/Test of using SDL (Simple Media Layer) library
;;;; using CFFI for foreign function interfacing...
;;;; (C)2006 Justin Heyes-Jones
;;;; see COPYING for license

;;;; To run this sample
;;;; (load "sdl_bmp_sample.lisp")
;;;; (sdl::bmp_sample) 

(in-package #:sdl-examples) 

;; #-:lispworks (defvar *bmp1-path* (merge-pathnames "examples/sdl.bmp" (or *load-truename* *default-pathname-defaults*)))
;; #-:lispworks (defvar *bmp2-path* (merge-pathnames "examples/lisp.bmp" (or *load-truename* *default-pathname-defaults*)))

;; #+:lispworks (defvar *bmp1-path* (merge-pathnames "sdl.bmp" (or *load-truename* *default-pathname-defaults*)))
;; #+:lispworks (defvar *bmp2-path* (merge-pathnames "lisp.bmp" (or *load-truename* *default-pathname-defaults*)))

(defvar *bmp1-path* (merge-pathnames "sdl.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *bmp2-path* (merge-pathnames "lisp.bmp" (or *load-truename* *default-pathname-defaults*)))


; window or screen height
(defparameter *SCREEN-WIDTH* 640)
(defparameter *SCREEN-HEIGHT* 480)
(defparameter *display-surface* nil)
(defparameter *loaded-bmps* nil)

; utilities used in this sample

; TODO If you want to take advantage of hardware colorkey or alpha blit acceleration, you should set the colorkey and alpha value before calling this.
(defun load-bmps(lst)
  "loads in the list of filenames (must be bmp files), and creates a display surface friendly surface for each of them"
  (loop for filename in lst do
	(let ((bmp-surface (sdl::load-bmp filename)))
	  (let ((display-surface (sdl::convert-surface-to-display-format bmp-surface)))
	    (setf *loaded-bmps* (cons display-surface *loaded-bmps*))))))

(defun close-bmps()
  "free up the surfaces we loaded the bmps into"
  (loop for surface in *loaded-bmps* do
	(sdl::SDL_FreeSurface surface))
  (setf *loaded-bmps* nil))


(defun bmp_sample () "demonstrates how to manage and display images from .bmp files"
  (sdl::with-init ()
    (let ((display (sdl::set-window 640 480)))
      (load-bmps (list (namestring *bmp1-path*) (namestring *bmp2-path*)))
      (sdl::with-events
	(:quit t)
	(:idle
	 (sdl::blit-surface (first *loaded-bmps*) display 10 10)
	 (sdl::blit-surface (second *loaded-bmps*) display 300 10)
	 (sdl::update-surface display)))
      (close-bmps))))

