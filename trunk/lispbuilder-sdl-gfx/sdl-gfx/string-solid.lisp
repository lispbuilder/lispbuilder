
(in-package :lispbuilder-sdl-gfx)


(defun render-string-solid (string &key
			    (font *default-font*)
			    (color sdl:*default-color*)
			    (free nil)
			    (cache nil))
  (when free
    (sdl:free-cached-surface font))
  (let ((surf (sdl:convert-surface :surface (sdl:create-surface (* (font-width font)
								   (length string))
								(font-height font)
								:key-color sdl:*black*)
				   :free-p t)))
    (draw-string-solid-* string 0 0
			 :font font
			 :surface surf
			 :color color)
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))

(defun draw-character-solid (c p1 &key
			     (font *default-font*)
			     (surface sdl:*default-surface*)
			     (color sdl:*default-color*))
  "See [DRAW-CHARACTER-SOLID-*](#draw-character-solid-*).

##### Parameters

* `P1` is the `X` and `Y` coordinates to render the character onto `SURFACE`, 
of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-character-solid-* c (sdl:x p1) (sdl:y p1)
			  :font font
			  :surface surface
			  :color color))

(defun draw-character-solid-* (c x y &key
			       (font *default-font*)
			       (surface sdl:*default-surface*)
			       (color sdl:*default-color*))
  "Draw character `C` at location `X` `Y` using font `FONT` with text color `COLOR` onto surface `SURFACE`.
The character is keyed over `SURFACE`.

##### Parameters

* `C` is the character to render. 
* `X` and `Y` are the x and y position coordinates, as `INTEGERS`.
* `FONT` is the font face used to render the character. Of type `FONT`.  Bound to `\*DEFAULT-FONT\*` if unspecified. 
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. Bound to `SDL:\*DEFAULT-SURFACE\*` if unspecified.
* `COLOR` color is the character color, of type `SDL:COLOR`.

##### Returns

* Returns the surface `SURFACE`.

##### Example

    \(DRAW-CHARACTER-SOLID-* \"Hello World!\" 0 0 :SURFACE A-SURFACE :COLOR A-COLOR\)"
  (check-type font font)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)

  (unless (default-font-p font)
    (set-default-font font))
  
  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::character-color (sdl:fp surface) x y c
				   (sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::character-RGBA (sdl:fp surface) x y c
				  (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color)))
  surface)

(defun draw-string-solid (string p1 &key
			  (font *default-font*)
			  (surface sdl:*default-surface*)
			  (color sdl:*default-color*))
  (check-type p1 sdl:point)
  (draw-string-solid-* string (sdl:x p1) (sdl:y p1)
		       :font font
		       :surface surface
		       :color color))

(defun draw-string-solid-* (string x y &key
			    (font *default-font*)
			    (surface sdl:*default-surface*)
			    (color sdl:*default-color*))
  (check-type font font)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)

  (unless (default-font-p font)
    (set-default-font font))

  (when (typep color 'sdl:color)
    (sdl-gfx-cffi::string-color (sdl:fp surface) x y string
				(sdl:pack-color color)))
  (when (typep color 'sdl:color-a)
    (sdl-gfx-cffi::string-RGBA (sdl:fp surface) x y string
			       (sdl:r color) (sdl:g color) (sdl:b color) (sdl:a color)))
  surface)
