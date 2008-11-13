;;;; General font definition

(in-package :lispbuilder-sdl)

(defclass sdl-font ()
  ((cached-surface :accessor cached-surface :initform nil))
  (:documentation
   "The generic SDL font class. All fonts in `LISPBUILDER-SDL` inherit from this class.
Free using [FREE](#free)."))

(defmethod width ((font sdl-font))
  (width (cached-surface font)))

(defmethod height ((font sdl-font))
  (height (cached-surface font)))

(defmethod x ((font sdl-font))
  (x (cached-surface font)))
(defmethod (setf x) (x-val (font sdl-font))
  (setf (x (cached-surface font)) x-val))

(defmethod y ((font sdl-font))
  (y (cached-surface font)))
(defmethod (setf y) (y-val (font sdl-font))
  (setf (y (cached-surface font)) y-val))

(defmethod draw-font (&key (font *default-font*) (surface *default-surface*))
(check-type font sdl-font)
  (blit-surface (cached-surface font) surface))

(defmethod draw-font-at (position &key (font *default-font*) (surface *default-surface*))
  (check-type font sdl-font)
  (draw-surface-at (cached-surface font) position :surface surface))

(defmethod draw-font-at-* (x y &key (font *default-font*) (surface *default-surface*))
  (check-type font sdl-font)
  (check-type (cached-surface font) sdl-surface)
  (draw-surface-at-* (cached-surface font) x y :surface surface))

(defmethod free-cached-surface ((font sdl-font))
  (when (cached-surface font)
    (free (cached-surface font))))
  