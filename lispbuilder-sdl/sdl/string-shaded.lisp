
(in-package :lispbuilder-sdl)

(defmethod _render-string-shaded_ :around ((string string) (fg-color color) (bg-color color) (font font) free cache)
  (when free
    (free-cached-surface font))
  (call-next-method))

(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font bitmap-font) free cache)
  (let ((surf (convert-surface :surface (create-surface (* (char-width font)
							   (length string))
							(char-height font))
			       :free t)))
    (_draw-string-shaded-*_ string 0 0 fg-color bg-color :LEFT surf font)
    (when cache
      (setf (cached-surface font) surf))
    surf))

(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color color) (bg-color color) justify (surface sdl-surface) (font bitmap-font))
  (case justify
    (:left (draw-string-left-justify-* string x y fg-color bg-color :surface surface :font font))
    (:right (draw-string-right-justify-* string x y fg-color bg-color :surface surface :font font))
    (:center (draw-string-centered-* string x y fg-color bg-color :surface surface :font font))
    (otherwise (error ":JUSTIFY must be one of :LEFT, :RIGHT or :CENTER")))
  surface)

(defun render-string-shaded (string fg-color bg-color &key
			    (font *default-font*)
			    (free nil)
			    (cache nil))
  "Render the string `STRING` using font `FONT` with text color `FG-COLOR` and background color `BG-COLOR`
to a new `SURFACE`. 
The dimensions of the new surface are height == `FONT` height, and width == `FONT` width * `STRING` length.
The surface background is filled with `BG-COLOR` so the surface cannot be keyed over other surfaces.
Use `:CACHE T` to cache the new surface in the `FONT` object.
When `:FREE T` any exisiting cached surface in `FONT` is automatically freed.
When `:FREE NIL` the caller is responsible for freeing any existing cached surface in `FONT`.

##### Parameters

* `STRING` is the text to render. 
* `FONT` is the font face used to render the `STRING`. Of type `FONT`.  Bound to 
`*DEFAULT-FONT*` if unspecified. 
* `FG-COLOR` color is the text color, of type `COLOR`
* `BG-COLOR` color is the background color used to fill the surface, of type `COLOR`
* `FREE` when `T` will free any exisiting cached surface in `FONT`.
* `CACHE` when `T` will cache the newly created SURFACE in `FONT`. Any cached surface can be accessed using
[CACHED-SURFACE](#cached-surface) and can be blitted to a target surface using [DRAW-FONT](#draw-font).

##### Returns

* Returns a new cached surface `SDL-SURFACE`.

##### Example

    \(RENDER-STRING-SHADED \"Hello World!\" F-COLOR B-COLOR\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  (_render-string-shaded_ string fg-color bg-color font free cache))

(defun draw-string-shaded (string p1 fg-color bg-color &key
			  (justify :left)
			  (surface *default-surface*)
			  (font *default-font*))
  "See [DRAW-STRING-SHADED-*](#draw-string-shaded-*).

##### Parameters

* `P1` is the `X` and `Y` coordinates to render the text, of type `SDL:POINT`.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  (_draw-string-shaded-*_ string (x p1) (y p1) fg-color bg-color justify (if surface surface *default-display*) font))

(defun draw-string-shaded-* (string x y fg-color bg-color &key
			    (justify :left)
			    (surface *default-surface*)
			    (font *default-font*))
  "Draw text `STRING` at location `X` `Y` using font `FONT` with text color `FG-COLOR` and background color `BG-COLOR`
onto surface `SURFACE`. 
The surface background is filled with `BG-COLOR` so the surface cannot be keyed over other surfaces.

* `STRING` is the text to render. 
* `X` and `Y` are the `X` and `Y` coordinates, as `INTEGERS`.
* `FG-COLOR` color is the text color, of type `COLOR`
* `BG-COLOR` color is the background color used to fill the surface `SURFACE`, of type `COLOR`
* `FONT` is the font face used to render the text. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL-SURFACE`. Bound to `\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-STRING-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE\)

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"
  (_draw-string-shaded-*_ string x y fg-color bg-color justify (if surface surface *default-display*) font))
