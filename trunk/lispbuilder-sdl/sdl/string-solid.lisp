
(in-package :lispbuilder-sdl)

(defmethod _render-string-solid_ :around ((string string) (font font) (color color) free cache)
  (when free
    (free-cached-surface font))
  (call-next-method))

(defmethod _render-string-solid_ ((string string) (font bitmap-font) (color color) free cache)
  (let ((surf (convert-surface :surface (create-surface (* (char-width font)
							   (length string))
							(char-height font)
							:color-key (any-color-but-this color)
							:type :hw)
			       :free t)))
    (_draw-string-solid-*_ string 0 0 :LEFT surf font color)
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defmethod _draw-string-solid-*_ ((string string) (x integer) (y integer) justify (surface sdl-surface) (font bitmap-font) (color color))
  (case justify
    (:left (draw-string-left-justify-* string x y color nil :surface surface :font font))
    (:right (draw-string-right-justify-* string x y color nil :surface surface :font font))
    (:center (draw-string-centered-* string x y color nil :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER")))
  surface)

(defun render-string-solid (string &key
                                   (font *default-font*)
                                   (color *default-color*)
                                   (free nil)
                                   (cache nil))
  "Render the string `STRING` using font `FONT` with text color `COLOR` to a new `SURFACE`. 
The dimensions of the new surface are height == `FONT` height, and width == `FONT` width * `STRING` length.
The surface background is transparent and therefore can be keyed over other surfaces.
Use `:CACHE T` to cache the new surface in the `FONT` object.
When `:FREE T` any exisiting cached surface in `FONT` is automatically freed.
When `:FREE NIL` the caller is responsible for freeing any existing cached surface in `FONT`.

##### Parameters

* `STRING` is the text to render. 
* `FONT` is the font face used to render the `STRING`. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `COLOR` color is the text color, of type `COLOR`.
* `FREE` when `T` will free any exisitng cached surface in `FONT`.
* `CACHE` when `T` will cache the newly created SURFACE in `FONT`. Any cached surface can be accessed using
[CACHED-SURFACE](#cached-surface) and can be blitted to a target surface using [DRAW-FONT](#draw-font).

##### Returns

* Returns a new cached surface `SDL-SURFACE`.

##### Example

    \(DRAW-STRING-SOLID \"Hello World!\" :COLOR A-COLOR\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"
  (_render-string-solid_ string font color free cache))

(defun draw-string-solid (string p1 &key
                                 (justify :left)
                                 (surface *default-surface*)
                                 (font *default-font*)
                                 (color *default-color*))
  "See [DRAW-STRING-SOLID-*](#draw-string-solid-*).

##### Parameters

* `P1` is the `X` and `X` coordinates to render the text, of type `POINT`.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"
  (_draw-string-solid-*_ string (x p1) (y p1) justify (if surface surface *default-display*) font color))

(defun draw-string-solid-* (string x y &key
                                   (justify :left)
                                   (surface *default-surface*)
                                   (font *default-font*)
                                   (color *default-color*))
  "Draw text `STRING` at location `X` `Y` using font `FONT` with color `COLOR` onto surface `SURFACE`.
The text is keyed onto `SURFACE`.

##### Parameters

* `STRING` is the text to render. 
* `X` and `Y` are the `X` and `Y` position coordinates, as `INTEGERS`.
* `FONT` is the font face used to render the string. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` color is the text color, of type `COLOR`.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-STRING-SOLID-* \"Hello World!\" 0 0 :SURFACE A-SURFACE :COLOR A-COLOR\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"
  (_draw-string-solid-*_ string x y justify (if surface surface *default-display*) font color))
