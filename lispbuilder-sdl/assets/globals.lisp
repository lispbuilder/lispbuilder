
(in-package #:lispbuilder-sdl)

;(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *default-font-path* (make-pathname
                             :host (pathname-host #.(or *compile-file-truename*
                                                        *load-truename*))
                             :directory (pathname-directory #.(or *compile-file-truename*
                                                                  *load-truename*))))

(defvar *default-simple-font* (merge-pathnames "font.bmp" *default-font-path*))
