
(in-package #:lispbuilder-sdl)

(export '*ttf-font-vera* :lispbuilder-sdl)

(defparameter *ttf-font-vera*
  (make-instance 'ttf-font-definition
                 :size 32
                 :filename (merge-pathnames "Vera.ttf" *default-font-path*)))

