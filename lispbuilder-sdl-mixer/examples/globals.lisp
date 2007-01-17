;;;; Globals for mixer exampels

(in-package #:sdl-mixer-examples) 

(defvar *audio-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
							     *load-truename*))
                                  :directory (pathname-directory #.(or *compile-file-truename*
                                                                       *load-truename*))))
