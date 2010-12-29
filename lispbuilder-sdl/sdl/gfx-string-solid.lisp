
(in-package :lispbuilder-sdl)

(defmethod _draw-string-solid-*_ ((string string) (x integer) (y integer) justify (surface sdl:sdl-surface) (font gfx-bitmap-font) (color sdl:color))
  (unless (default-font-p font)
    (set-default-font font))
  (let ((x-pos x))
    (if (eq justify :right)
      (setf x-pos (- x-pos (* (char-width font) (length string)))))
    (if (eq justify :center)
      (setf x-pos (- x-pos (/ (* (char-width font) (length string)) 2))))

    (gfx-string-color x-pos y string :surface surface :color color))
  surface)

(defun draw-character-solid (c p1 &key
                               (font sdl:*default-font*)
                               (surface sdl:*default-surface*)
                               (color sdl:*default-color*)
                               (gfx-loaded-p *gfx-loaded-p*))
  "See [DRAW-CHARACTER-SOLID-*](#draw-character-solid-*).

##### Parameters

* `P1` is the `X` and `Y` coordinates to render the character onto `SURFACE`, 
of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-character-solid-* c (sdl:x p1) (sdl:y p1)
			  :font font
			  :surface surface
			  :color color :gfx-loaded-p gfx-loaded-p))

(defun draw-character-solid-* (c x y &key
                                 (font sdl:*default-font*)
                                 (surface sdl:*default-surface*)
                                 (color sdl:*default-color*) (gfx-loaded-p *gfx-loaded-p*))
  (if gfx-loaded-p
    (draw-character-solid-* c x y :font font :surface surface :color color)))

(defun gfx-draw-character-solid-* (c x y &key
                                     (font sdl:*default-font*)
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
  (check-type font bitmap-font)
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (check-type color sdl:color)

  (unless (sdl:default-font-p font)
    (sdl:set-default-font font))

  (gfx-character-color x y c :surface surface :color color)
  surface)
