
(in-package #:sdl-examples) 

;;(defvar *bmp-path* (or *load-truename* *default-pathname-defaults*))
(defvar *bmp-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							     *load-truename*))
                                  :directory (pathname-directory #.(or *compile-file-truename*
                                                                       *load-truename*))))
; set load path
;(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							      *load-truename*))
				   :directory (pathname-directory #.(or *compile-file-truename*
									*load-truename*))))
