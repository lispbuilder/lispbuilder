
(in-package #:sdl-mixer-cffi)


(defmethod cffi:translate-to-foreign (value (type (eql 'sdl-mixer-cffi::chunk)))
  (fp value))

(defmethod cffi:translate-to-foreign (value (type (eql 'sdl-mixer-cffi::music)))
  (fp value))

(defun to-mix-chunk (value)
  (fp value))

(defun to-mix-music (value)
  (fp value))