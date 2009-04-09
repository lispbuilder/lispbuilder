
(in-package #:rm-examples) 

(defvar *image-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							       *load-truename*))
				    :directory (pathname-directory #.(or *compile-file-truename*
									 *load-truename*))))

