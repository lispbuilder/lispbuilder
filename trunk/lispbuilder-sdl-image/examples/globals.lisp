
(in-package #:sdl-image-examples)

;;(defvar *bmp-path* (or *load-truename* *default-pathname-defaults*))
(defvar *bmp-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							     *load-truename*))
                                  :directory (pathname-directory #.(or *compile-file-truename*
                                                                       *load-truename*))))
