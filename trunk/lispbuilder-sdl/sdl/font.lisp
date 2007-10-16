;;;; General font definition

(in-package :lispbuilder-sdl)

(defclass sdl-font ()
  ((cached-surface :accessor cached-surface :initform nil))
  (:documentation
   "The generic SDL font class. All fonts used in SDL need to inherit from this class."))

(defmethod fp ((font sdl-font))
  "Returns the cached surface `SURFACE` in `FONT`, or `NIL` if the font does not contain
a cached surface."
  (fp (cached-surface font)))

(defmethod fp-position ((font sdl-font))
  "Returns the `X` and `Y` coordinates of the cached surface `SURFACE` in `FONT`, as `POINT`."
  (fp-position (cached-surface font)))

(defmethod width ((font sdl-font))
  "Returns the `INTEGER` width of the cached surface `SURFACE` in `FONT`."  
  (sdl-base::surf-w (fp (cached-surface font))))
(defmethod (setf width) (w-val (font sdl-font))
  "Sets the `INTEGER` width of the cached surface `SURFACE` in `FONT`."
  (setf (sdl-base::rect-w (fp-position (cached-surface font))) w-val))

(defmethod height ((font sdl-font))
  "Returns the `INTEGER` height of the cached surface `SURFACE` in `FONT`."
  (sdl-base::surf-h (fp (cached-surface font))))
(defmethod (setf height) (h-val (font sdl-font))
  "Sets the `INTEGER` height of the cached surface `SURFACE` in `FONT`."
  (setf (sdl-base::rect-h (fp-position (cached-surface font))) h-val))

(defmethod x ((font sdl-font))
  "Returns the `X` position of the cached surface `SURFACE` in `FONT`."
  (sdl-base::rect-x (fp-position (cached-surface font))))
(defmethod (setf x) (x-val (font sdl-font))
  "Sets the `INTEGER` `X` position of the cached surface `SURFACE` in `FONT`."
  (setf (sdl-base::rect-x (fp-position (cached-surface font))) x-val))

(defmethod y ((font sdl-font))
  "Returns the `Y` position of the cached surface `SURFACE` in `FONT`."  
  (sdl-base::rect-y (fp-position (cached-surface font))))
(defmethod (setf y) (y-val (font sdl-font))
  "Sets the `INTEGER` `Y` position of the cached surface `SURFACE` in `FONT`."
  (setf (sdl-base::rect-y (fp-position (cached-surface font))) y-val))

(defmethod free-cached-surface ((font sdl-font))
"Frees resources allocated to the cached surface `SURFACE` in `FONT`, if any. 
Sets the `FONT`s `CACHED-SURFACE` slot to NIL."
  (when (and (cached-surface font)
	     (typep (cached-surface font) 'sdl-surface))
    (free-surface (cached-surface font))
    (setf (cached-surface font) nil)))

(defmethod draw-font (&key (font *default-font*) (surface *default-surface*))
  "Blit the cached surface in the font `FONT` to 
the destination surface `SURFACE`. The cached surface is created during a previous call to any of 
the DRAW-STRING* functions. Uses the `POSITION` of the cached surface to render at X/Y coordinates 
on the destination `SURFACE`. This function can provide a speed increase upon redraws when the text in `FONT` 
remains unchanged between screen updates."
(check-type font sdl-font)
  (blit-surface (cached-surface font) surface))

(defmethod draw-font-at (position &key (font *default-font*) (surface *default-surface*))
  "See [DRAW-FONT](#draw-font). 

* `POINT` is the X/Y position of the the font's cached surface. `POINT` is of type `POINT`."
  (check-type font sdl-font)
  (draw-surface-at (cached-surface font) position :surface surface))

(defmethod draw-font-at-* (x y &key (font *default-font*) (surface *default-surface*))
  "See [DRAW-FONT](#draw-font). 

##### Parameters

* `POINT` is the `X` and `Y` coordinates of the the `FONT`s cached surface, of type `POINT`."
  (check-type font sdl-font)
  (check-type (cached-surface font) sdl-surface)
  (draw-surface-at-* (cached-surface font) x y :surface surface))

(defmethod free-font ((font sdl-font))
"Free's the resources allocated to the `FONT`."
  (tg:cancel-finalization font)
  (free-cached-surface font))
