;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-base)


(defun create-RWops-from-file (filename)
  (let ((file-path (namestring filename)))
    (if (and (stringp file-path) (probe-file file-path))
	(let ((rwops (sdl-cffi::sdl-RW-From-File file-path "rb")))
	  (if (is-valid-ptr rwops)
	      rwops
	      nil))
	nil)))

