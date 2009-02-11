
(in-package #:lispbuilder-sdl)

(defparameter *simple-font-4x5*
  (make-instance 'simple-font-definition
                 :width 4 :height 5
                 :character-map "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789"
                 :color-key (sdl:color :r 99 :g 0 :b 0)
                 :filename (create-path "font.bmp" *default-asset-path*)
                 :pad-x 0 :pad-y 0))

