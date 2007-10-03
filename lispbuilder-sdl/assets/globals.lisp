
(in-package #:lispbuilder-sdl)

(defvar *default-ttf-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to the default Truetype font, `\"Bitstream Vera\"`")

(defvar *default-ttf* (merge-pathnames "Vera.ttf" *default-ttf-path*))