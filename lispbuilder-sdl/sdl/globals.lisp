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
(defvar *default-color* #(0 0 0))
(defvar *default-position* #(0 0))
(defvar *default-rectangle* #(0 0 0 0))
(defvar *default-font* nil)

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
