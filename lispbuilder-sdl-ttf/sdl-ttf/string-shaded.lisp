;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl-ttf)

(defun render-string-shaded (text fg-color bg-color &key
			     (font *default-font*)
			     (free nil)
			     (cache nil))
  "Render text `TEXT` using font `FONT` with foreground and background colors `FG-COLOR` and `BG-COLOR` 
into a new `SURFACE`, using the Solid mode. 
Unless `:FREE T`, the caller is responsible for freeing the new `SURFACE`.
Use `:CACHE T` to cache the newly created surface in the `FONT` object.

##### Parameters

* `TEXT` is the text to render, of type `STRING`.
* `FG-COLOR` is the foreground color of the `TEXT`, of type `SDL:COLOR`
* `BG-COLOR` is the background color of the `TEXT`, of type `SDL:COLOR`
* `FONT` is a `FONT` object.  Binds to \*`DEFAULT-FONT\*` by default. 
* `FREE` when `T` will free the existing cached `SURFACE` in `FONT`.
* `CACHE` when `T` will cache the newly created `SURFACE` in `FONT`.

##### Returns

* Returns the new surface `SDL:SDL-SURFACE`.

##### Example

\(DRAW-STRING-SHADED \"Hello World!\" fg-col bg-col  :FONT *DEFAULT-FONT* :COLOR A-COLOR\)"
  (check-type font font)
  (sdl:check-types sdl:color fg-color bg-color)
  (when free
    (sdl:free-cached-surface font))
  (let ((surf
	 (sdl:with-foreign-color-copy (fg-struct fg-color)
	   (sdl:with-foreign-color-copy (bg-struct bg-color)
	     (make-instance 'sdl:surface :fp (sdl-ttf-cffi::ttf-Render-UTF8-shaded (sdl:fp font) text fg-struct bg-struct))))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))


(defun draw-string-shaded (text position fg-color bg-color &key
			   (font *default-font*)
			   (surface sdl:*default-surface*))
  "See [DRAW-STRING-SHADED-*](#draw-string-shaded-*)

##### Parmeters

* `POSITION` is the `X` and `Y` coordinates to render `TEXT`, of type `SDL:POINT`."
  (check-type position sdl:point)
  (draw-string-shaded-* text (sdl:x position) (sdl:y position) fg-color bg-color
			:font font
			:surface surface))

(defun draw-string-shaded-* (text x y fg-color bg-color &key
			     (font *default-font*)
			     (surface sdl:*default-surface*))
  "Draw text `TEXT` using font `FONT` with foreground color `FG-COLOR` and background color `BG-COLOR` 
onto surface `SURFACE`, using the Shaded mode. 

##### Parameters

* `TEXT` is the text to render, of type `STRING`.
* `X` and `Y` are the x and y coordinates of the `TEXT`, as `INTEGER`s.
* `FG-COLOR` is the foreground color of the `TEXT`, of type `SDL:COLOR`
* `BG-COLOR` is the background color of the `TEXT`, of type `SDL:COLOR`
* `FONT` is a `FONT` object.  Binds to `\*DEFAULT-FONT\*` by default. 
* `SURFACE` is the target surface, of type `SDL:SDL-SURFACE`. 

##### Returns

* Returns the target surface `SURFACE`.

##### Example

    \(DRAW-STRING-SHADED-* \"Hello World!\" 0 0 fg-col bg-col :FONT *DEFAULT-FONT* :SURFACE A-SURFACE\)"
  (unless surface
    (setf surface sdl:*default-display*))
  (check-type surface sdl:sdl-surface)
  (sdl:with-surface (font-surface (render-string-shaded text fg-color bg-color
							:font font
							:cache nil
							:free nil)
				  t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  surface)
