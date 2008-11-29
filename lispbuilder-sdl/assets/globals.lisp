
(in-package #:lispbuilder-sdl)

;(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to any fonts used in teh `LISPBUILDER-SDL` packages.")

(defvar *default-ttf* (merge-pathnames "Vera.ttf" *default-font-path*))

