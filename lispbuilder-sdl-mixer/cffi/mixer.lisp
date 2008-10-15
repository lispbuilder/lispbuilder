
(in-package #:sdl-mixer-cffi)

(defclass chunk (sdl::foreign-object) ()
  (:default-initargs
   :gc t
    :free #'sdl-mixer-cffi::free-chunk))

(defclass music (sdl::foreign-object) ()
  (:default-initargs
   :gc t
    :free #'sdl-mixer-cffi::free-music))

