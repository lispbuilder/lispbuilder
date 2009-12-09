
(in-package #:lispbuilder-sdl)

(defparameter *simple-font-4x5*
  (make-instance 'sdl:simple-font-definition
                 :width 4 :height 5
                 :character-map "abcdefghijklmnopqrstuvwxyz:'!?_-,.()#~0123456789"
                 :character-mask (loop for y from 0 below 1
                                       append (loop for x from 0 below 48
                                                    collect (list (* x 4) (* y 5) 4 5)))
                 :color-key (sdl:color :r 99 :g 0 :b 0)
                 :filename (sdl:create-path "font.bmp" sdl:*default-asset-path*)
                 :loader #'load-image))

