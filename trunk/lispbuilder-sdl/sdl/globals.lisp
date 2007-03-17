;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Globals

(defvar *default-surface* nil)
(defvar *default-display* nil)
(defvar *default-color* nil)
(defvar *default-position* nil)
(defvar *default-rectangle* nil)
(defvar *default-font* nil)

;(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*))))
;; (defvar *renderer* nil)
;; (defvar *quit* nil)

(defvar *sdl-initialized* nil)
(defvar *sdl-init-on-startup* nil)
(defvar *sdl-quit-on-exit* nil)
(defvar *initialize-on-startup* (logior SDL-INIT-VIDEO))
(defvar *quit-on-exit* (logior SDL-INIT-VIDEO))

(defvar *external-init-on-startup* nil
  "The list of functions that are called from [INIT-SDL](#init-sdl).")
(defvar *external-quit-on-exit* nil
  "The list of functions that are called from [QUIT-SDL](#quit-sdl).")

(defun default-surface ()
  *default-surface*)
(defun (setf default-surface) (surface)
  (setf *default-surface* surface))

(defun default-display ()
  *default-display*)
(defun (setf default-display) (surface)
  (setf *default-display* surface))

(defun default-color ()
  *default-color*)
(defun (setf default-color) (color)
  (setf *default-color* color))

(defun default-position ()
  *default-position*)
(defun (setf default-position) (position)
  (setf *default-position* position))

(defun default-rectangle ()
  *default-rectangle*)
(defun (setf default-rectangle) (rectangle)
  (setf *default-rectangle* rectangle))


;; (declaim (INLINE renderer))
;; (defun renderer ()
;;   *renderer*)
;; (defsetf renderer set-renderer)
;; (defun set-renderer (renderer)
;;   (setf *renderer* renderer))
