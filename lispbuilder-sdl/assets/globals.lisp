
(in-package #:lispbuilder-sdl-assets)

(defparameter *default-asset-path*
  (make-pathname
   :host (pathname-host #.(or *compile-file-truename*
                              *load-truename*))
   :directory (pathname-directory #.(or *compile-file-truename*
                                        *load-truename*))))


