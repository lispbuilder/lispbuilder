
(in-package :lispbuilder-windows) 

(cffi:define-foreign-library kernel32
  (:windows "kernel32.dll"))
(cffi:define-foreign-library user32
  (:windows "user32.dll"))
(cffi:define-foreign-library gdi32
  (:windows "gdi32.dll"))

(cffi:use-foreign-library kernel32)
(cffi:use-foreign-library user32)
(cffi:use-foreign-library gdi32)



