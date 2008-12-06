
(in-package #:sdl-bin)

(defvar *dll-path* (make-pathname
                    :host (pathname-host #.(or *compile-file-truename*
                                               *load-truename*))
                    :directory (pathname-directory #.(or *compile-file-truename*
                                                         *load-truename*))))

