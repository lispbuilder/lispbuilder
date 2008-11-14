;;;; General font definition

(in-package :lispbuilder-sdl)

(defclass font ()
  ((cached-surface :accessor cached-surface :initform nil))
  (:documentation
   "The generic SDL font class. All fonts in `LISPBUILDER-SDL` inherit from this class.
Free using [FREE](#free)."))

(defmethod width ((font font))
  (width (cached-surface font)))

(defmethod height ((font font))
  (height (cached-surface font)))

(defmethod x ((font font))
  (x (cached-surface font)))
(defmethod (setf x) (x-val (font font))
  (setf (x (cached-surface font)) x-val))

(defmethod y ((font font))
  (y (cached-surface font)))
(defmethod (setf y) (y-val (font font))
  (setf (y (cached-surface font)) y-val))

(defmethod draw-font (&key (font *default-font*) (surface *default-surface*))
(check-type font font)
  (blit-surface (cached-surface font) surface))

(defmethod draw-font-at (position &key (font *default-font*) (surface *default-surface*))
  (check-type font font)
  (draw-surface-at (cached-surface font) position :surface surface))

(defmethod draw-font-at-* (x y &key (font *default-font*) (surface *default-surface*))
  (check-type font font)
  (check-type (cached-surface font) sdl-surface)
  (draw-surface-at-* (cached-surface font) x y :surface surface))

(defmethod free-cached-surface ((font font))
  (when (cached-surface font)
    (free (cached-surface font))))

(defmacro with-default-font ((font) &body body)
  "Sets `\*DEFAULT-FONT\*` to `FONT` within the scope of `WITH-DEFAULT-FONT`.

##### Example

    \(WITH-DEFAULT-FONT \(new-font\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  `(let ((*default-font* ,font))
     ,@body))

(defmacro with-font ((font font-definition) &body body)
  "Sets `\*DEFAULT-FONT\*` to a new [BITMAP-FONT](#bitmap-font) in `FONT` within the scope of `WITH-FONT`.
Frees `FONT` when `WITH-FONT` goes out of scope.

##### Example

    \(WITH-FONT \(new-font *font-8x8*\)
        \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR\)\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  `(let ((,font (initialise-font ,font-definition)))
     (with-default-font (,font)
       ,@body)
     (free ,font)))

