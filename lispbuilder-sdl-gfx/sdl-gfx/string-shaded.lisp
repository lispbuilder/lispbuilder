
(in-package :lispbuilder-sdl-gfx)

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
* `FONT` is the font face used to render the `STRING`. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `FG-COLOR` color is the text color, of type `SDL:SDL-COLOR`
* `BG-COLOR` color is the background color used to fill the surface, of type `SDL:SDL-COLOR`
* `FREE` when `T` will free any exisiting cached surface in `FONT`.
* `CACHE` when `T` will cache the newly created SURFACE in `FONT`.

##### Returns

* Returns a new surface `SDL:SDL-SURFACE`.

##### Example

    \(DRAW-STRING-SHADED \"Hello World!\" F-COLOR B-COLOR\)"
  (sdl:check-types sdl:sdl-color fg-color bg-color)
  (when free
    (sdl:free-cached-surface font))
  (let ((surf (sdl:convert-surface :surface (sdl:create-surface (* (font-width font)
								   (length string))
								(font-height font))
				   :free t)))
    (draw-string-shaded-* string 0 0 fg-color bg-color
			  :font font
			  :surface surf)
    (when cache 
      (setf (sdl:cached-surface font) surf))
    surf))

(defun draw-character-shaded (c p1 fg-color bg-color &key
			      (font *default-font*)
			      (surface sdl:*default-surface*))
  "See [DRAW-CHARACTER-SHADED-*](#draw-character-shaded-*).

##### Parameters

* `P1` is the x and y position to render the character, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-character-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color
			   :font font
			   :surface surface))

(defun draw-character-shaded-* (c x y fg-color bg-color &key
				(font *default-font*)
				(surface sdl:*default-surface*))
  "Draw the character `C` at location `X` `Y` using font `FONT` with text color `FG-COLOR` and background color `BG-COLOR`
onto surface `SURFACE`. 
The surface background is filled with `BG-COLOR` so the surface cannot be keyed over other surfaces.

##### Parameters

* `C` is the character to render. 
* `X` and `Y` are the x and y position coordinates, as `INTEGERS`.
* `FG-COLOR` color is the character color, of type `SDL:SDL-COLOR`
* `BG-COLOR` color is the background color used to fill the surface `SURFACE`, of type `SDL:SDL-COLOR`
* `FONT` is the font face used to render the character. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE\)"
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (sdl:check-types sdl:sdl-color fg-color bg-color)

  (unless (default-font-p font)
    (set-default-font font))
  
  (sdl:draw-box-* x y
		  (* (font-width font)
		     (length c))
		  (font-height font)
		  :color bg-color
		  :surface surface)
  
  (when (typep fg-color 'sdl:color)
    (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				   (sdl:pack-color fg-color)))
  (when (typep fg-color 'sdl:color-a)
    (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				  (sdl:r fg-color) (sdl:g fg-color) (sdl:b fg-color) (sdl:a fg-color)))
  surface)

(defun draw-string-shaded (c p1 fg-color bg-color &key
			   (font *default-font*)
			   (surface sdl:*default-surface*))
  "See [DRAW-STRING-SHADED-*](#draw-string-shaded-*).

##### Parameters

* `P1` is the `X` and `Y` coordinates to render the text, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-string-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color
			:font font
			:surface surface))

(defun draw-string-shaded-* (c x y fg-color bg-color &key
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "Draw text `C` using at location `X` `Y` using font `FONT` with text color `FG-COLOR` and background color `BG-COLOR`
onto surface `SURFACE`. 
The surface background is filled with `BG-COLOR` so the surface cannot be keyed over other surfaces.

* `C` is the text to render. 
* `X` and `Y` are the `X` and `Y` coordinates coordinates, as `INTEGERS`.
* `FG-COLOR` color is the text color, of type `SDL:SDL-COLOR`
* `BG-COLOR` color is the background color used to fill the surface `SURFACE`, of type `SDL:SDL-COLOR`
* `FONT` is the font face used to render the text. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-STRING-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE\)"
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (sdl:check-types sdl:sdl-color fg-color bg-color)

  (unless (default-font-p font)
    (set-default-font font))
  
  (sdl:draw-box-* x y
		  (* (font-width font)
		     (length c))
		  (font-height font)
		  :color bg-color
		  :surface surface)
  (when (typep fg-color 'sdl:color)
    (sdl-gfx-cffi::string-color (sdl:fp surface) x y c
				(sdl:pack-color fg-color)))
  (when (typep fg-color 'sdl:color-a)
    (sdl-gfx-cffi::string-RGBA (sdl:fp surface) x y c
			       (sdl:r fg-color) (sdl:g fg-color) (sdl:b fg-color) (sdl:a fg-color)))
  surface)
