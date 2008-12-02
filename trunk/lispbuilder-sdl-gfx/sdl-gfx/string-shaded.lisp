
(in-package :lispbuilder-sdl)

(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color sdl:color) (bg-color sdl:color) justify (surface sdl:sdl-surface) (font gfx-bitmap-font))
  (unless (default-font-p font)
    (sdl:set-default-font font))
  
  (draw-box-* x y
	      (* (char-width font)
		 (length string))
	      (char-height font)
	      :color bg-color
	      :surface surface)
  (sdl-gfx-cffi::string-color (fp surface) x y string
			      (pack-color fg-color))
  surface)

(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color sdl:color-a) (bg-color sdl:color) justify (surface sdl:sdl-surface) (font gfx-bitmap-font))
  (unless (default-font-p font)
    (sdl:set-default-font font))
  
  (draw-box-* x y
	      (* (char-width font)
		 (length string))
	      (char-height font)
	      :color bg-color
	      :surface surface)
  (sdl-gfx-cffi::string-RGBA (fp surface) x y string
			     (r fg-color) (g fg-color) (b fg-color) (a fg-color))
  surface)

(in-package :lispbuilder-sdl-gfx)

(defun draw-character-shaded (c p1 fg-color bg-color &key
			      (font sdl:*default-font*)
			      (surface sdl:*default-surface*))
  "See [DRAW-CHARACTER-SHADED-*](#draw-character-shaded-*).

##### Parameters

* `P1` is the x and y position to render the character, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-character-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color
			   :font font
			   :surface surface))

(defun draw-character-shaded-* (c x y fg-color bg-color &key
				(font sdl:*default-font*)
				(surface sdl:*default-surface*))
  "Draw the character `C` at location `X` `Y` using font `FONT` with text color `FG-COLOR` and background color `BG-COLOR`
onto surface `SURFACE`. 
The surface background is filled with `BG-COLOR` so the surface cannot be keyed over other surfaces.

##### Parameters

* `C` is the character to render. 
* `X` and `Y` are the x and y position coordinates, as `INTEGERS`.
* `FG-COLOR` color is the character color, of type `SDL:COLOR`
* `BG-COLOR` color is the background color used to fill the surface `SURFACE`, of type `SDL:COLOR`
* `FONT` is the font face used to render the character. Of type `FONT`.  Bound to `*DEFAULT-FONT*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-CHARACTER-SHADED-* \"Hello World!\" 0 0 F-COLOR B-COLOR :SURFACE A-SURFACE\)"
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (sdl:check-types sdl:color fg-color bg-color)

  (unless (sdl:default-font-p font)
    (sdl:set-default-font font))
  
  (sdl:draw-box-* x y
		  (* (sdl:char-width font)
		     (length c))
		  (sdl:char-height font)
		  :color bg-color
		  :surface surface)
  
  (when (typep fg-color 'sdl:color)
    (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				   (sdl:pack-color fg-color)))
  (when (typep fg-color 'sdl:color-a)
    (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				  (sdl:r fg-color) (sdl:g fg-color) (sdl:b fg-color) (sdl:a fg-color)))
  surface)


