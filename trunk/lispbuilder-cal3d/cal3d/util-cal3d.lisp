;; Cal#D library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; see COPYING for license
;; This file contains some useful functions for using Cal3D from Common lisp

(in-package #:lispbuilder-cal3d)


(defun get-ambient-color (renderer)
  (let ((meshcolor (cffi:foreign-alloc :uint8 :count 4 :initial-element 0))
	(color (vector 0 0 0 0)))
    (cal3d::CalRenderer_GetAmbientColor renderer (cffi::make-pointer (cffi:pointer-address meshcolor)))
    (dotimes (i 4)
      (setf (elt color i)
	    (/ (cffi:mem-aref meshcolor :uint8 i) 255.0)))
    (cffi:foreign-free meshcolor)
    color))

(defun get-diffuse-color (renderer)
  (let ((meshcolor (cffi:foreign-alloc :uint8 :count 4 :initial-element 0))
	(color (vector 0 0 0 0)))
    (cal3d::CalRenderer_getDiffuseColor renderer meshcolor)
    (dotimes (i 4)
      (setf (elt color i)
	    (/ (cffi:mem-aref meshcolor :uint8 i) 255.0)))
    (cffi:foreign-free meshcolor)
    color))

(defun get-specular-color (renderer)
  (let ((meshcolor (cffi:foreign-alloc :uint8 :count 4 :initial-element 0))
	(color (vector 0 0 0 0)))
    (cal3d::CalRenderer_getSpecularColor renderer meshcolor)
    (dotimes (i 4)
      (setf (elt color i)
	    (/ (cffi:mem-aref meshcolor :uint8 i) 255.0)))
    (cffi:foreign-free meshcolor)
    color))

(defun get-specular-exponent (renderer)
  (cal3d::CalRenderer_getShininess renderer))

