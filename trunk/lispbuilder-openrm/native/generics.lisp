
(in-package #:lispbuilder-openrm)

(defgeneric hwnd (object))
(defgeneric (setf hwnd) (value object))

(defgeneric update-window (obj))
(defgeneric show-window (obj))
