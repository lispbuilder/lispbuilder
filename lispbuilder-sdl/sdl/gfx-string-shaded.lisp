
(in-package :lispbuilder-sdl)

(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color sdl:color) (bg-color sdl:color) justify (surface sdl:sdl-surface) (font gfx-bitmap-font))
  (unless (default-font-p font)
    (sdl:set-default-font font))

  (let ((x-pos x))
    (if (eq justify :right)
      (setf x-pos (- x-pos (* (char-width font) (length string)))))
    (if (eq justify :center)
      (setf x-pos (- x-pos (/ (* (char-width font) (length string)) 2))))
    (draw-box-* x-pos y
                (* (char-width font)
                   (length string))
                (char-height font)
                :color bg-color
                :surface surface)
    (gfx-string-color x-pos y string :surface surface :color fg-color))
  surface)

(defun draw-character-shaded (c p1 fg-color bg-color &key
			      (font sdl:*default-font*)
			      (surface sdl:*default-surface*) (gfx-loaded-p *gfx-loaded-p*))
  "See [DRAW-CHARACTER-SHADED-*](#draw-character-shaded-*).

##### Parameters

* `P1` is the x and y position to render the character, of type `SDL:POINT`."
  (check-type p1 sdl:point)
  (draw-character-shaded-* c (sdl:x p1) (sdl:y p1) fg-color bg-color
			   :font font
			   :surface surface :gfx-loaded-p gfx-loaded-p))

(defun draw-character-shaded-* (c x y fg-color bg-color &key
                                  (font sdl:*default-font*)
                                  (surface sdl:*default-surface*) (gfx-loaded-p *gfx-loaded-p*))
  "See [DRAW-CHARACTER-SHADED-*](#draw-character-shaded-*).

##### Parameters

* `P1` is the x and y position to render the character, of type `SDL:POINT`."
  (if gfx-loaded-p
    (gfx-draw-character-shaded-* c x y fg-color bg-color
                                 :font font
                                 :surface surface)))

(defun gfx-draw-character-shaded-* (c x y fg-color bg-color &key
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

  (gfx-character-color x y c :surface surface :color fg-color)
  surface)
