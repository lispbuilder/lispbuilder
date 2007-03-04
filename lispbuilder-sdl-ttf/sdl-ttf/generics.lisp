
(in-package #:lispbuilder-sdl-ttf)


(defgeneric x (font)
  (:documentation "Returns the top left `X` coordinate of the font as an `INTEGER`."))

(defgeneric y (font)
  (:documentation "Returns the top left `Y` coordinate of the font as an `INTEGER`."))

(defgeneric (setf x) (value font)
  (:documentation "Sets the top left `X` coordinate of the font."))

(defgeneric (setf y) (value font)
  (:documentation "Sets the top left `Y` coordinate of the font."))

(defgeneric fp-position (font)
  (:documentation "Returns the foreign `SDL_Rect` that determines the position of the `FONT`."))

