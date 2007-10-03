
(in-package #:lispbuilder-sdl)

(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to the default Truetype font, `\"Bitstream Vera\"`")

(defvar *default-font* (merge-pathnames "Vera.ttf" *default-font-path*))