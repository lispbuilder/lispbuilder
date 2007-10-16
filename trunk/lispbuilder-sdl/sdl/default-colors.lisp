
(in-package #:lispbuilder-sdl)

;;; Some useful colors from http://en.wikipedia.org/wiki/RGB_color_model

(defvar *black* (color :r 0 :g 0 :b 0)
  "The `COLOR` black.")
(defvar *white* (color :r 255 :g 255 :b 255)
  "The `COLOR` white.")
(defvar *red* (color :r 255 :g 0 :b 0)
  "The `COLOR` red.")
(defvar *green* (color :r 0 :g 255 :b 0)
  "The `COLOR` green.")
(defvar *blue* (color :r 0 :g 0 :b 255)
  "The `COLOR` blue.")
(defvar *yellow* (color :r 255 :g 255 :b 0)
  "The `COLOR` yellow.")
(defvar *cyan* (color :r 0 :g 255 :b 255)
  "The `COLOR` cyan.")
(defvar *magenta* (color :r 255 :g 0 :b 255)
  "The `COLOR` magenta.")
