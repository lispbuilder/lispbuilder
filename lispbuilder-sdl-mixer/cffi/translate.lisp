
(in-package #:sdl-mixer-cffi)


;;(defmethod cffi:translate-to-foreign (value (type (eql 'sdl-mixer-cffi::chunk)))
;;  (sdl:fp value))

;;(defmethod cffi:translate-to-foreign (value (type (eql 'sdl-mixer-cffi::music)))
;;  (sdl:fp value))

(defun to-mix-chunk (value)
  (sdl:fp value))

(defun to-mix-music (value)
  (sdl:fp value))