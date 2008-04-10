
(in-package #:sdl-mixer-cffi)

(defclass chunk (foreign-object) ()
  (:default-initargs
   :gc t
    :free (simple-free 'sdl-mixer-cffi::free-chunk 'chunk)))

(defclass music (foreign-object) ()
  (:default-initargs
   :gc t
    :free (simple-free 'sdl-mixer-cffi::free-music 'music)))

