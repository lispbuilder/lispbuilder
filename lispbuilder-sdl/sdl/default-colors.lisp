
(in-package #:lispbuilder-sdl)

;;; Some useful colors from http://en.wikipedia.org/wiki/RGB_color_model

(defvar *black* (color :r 0 :g 0 :b 0)
  "`RGB` [COLOR](#color) black.")
(defvar *white* (color :r 255 :g 255 :b 255)
  "`RGB` [COLOR](#color) white.")
(defvar *red* (color :r 255 :g 0 :b 0)
  "`RGB` [COLOR](#color) red.")
(defvar *green* (color :r 0 :g 255 :b 0)
  "`RGB` [COLOR](#color) green.")
(defvar *blue* (color :r 0 :g 0 :b 255)
  "`RGB` [COLOR](#color) blue.")
(defvar *yellow* (color :r 255 :g 255 :b 0)
  "`RGB` [COLOR](#color) yellow.")
(defvar *cyan* (color :r 0 :g 255 :b 255)
  "`RGB` [COLOR](#color) cyan.")
(defvar *magenta* (color :r 255 :g 0 :b 255)
  "`RGB` [COLOR](#color) magenta.")
