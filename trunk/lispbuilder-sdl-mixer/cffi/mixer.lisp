
(in-package #:sdl-mixer-cffi)

(defclass mix-chunk (foreign-object) ()
  (:default-initargs
   :gc t
    :free (simple-free 'sdl-mixer-cffi::mix-free-chunk 'mix-chunk)))

(defclass mix-music (foreign-object) ()
  (:default-initargs
   :gc t
    :free (simple-free 'sdl-mixer-cffi::mix-free-music 'mix-music)))

