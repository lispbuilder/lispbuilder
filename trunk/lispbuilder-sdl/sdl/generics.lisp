;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric fp (object)
  (:documentation "Returns the default foreign object for `OBJECT`."))

(defgeneric fp-position (object)
  (:documentation "Returns the default foreign SDL_Rect object for `OBJECT`."))

(defgeneric pack-color (color)
  (:documentation "Packs the color components in `COLOR` into a four byte `INTEGER`."))

(defgeneric r (color)
  (:documentation "Returns the red component of color `COLOR` as an `INTEGER`."))
(defgeneric g (color)
  (:documentation "Returns the green component of color `COLOR` as an `INTEGER`."))
(defgeneric b (color)
  (:documentation "Returns the blue component of color `COLOR` as an `INTEGER`."))
(defgeneric a (color)
  (:documentation "Returns the alpha component of color `COLOR` as an `INTEGER`."))

(defgeneric (setf r) (value color)
  (:documentation "Sets the red `R` component of color `COLOR`."))
(defgeneric (setf g) (value color)
  (:documentation "Sets the green `G` component of color `COLOR`."))
(defgeneric (setf b) (value color)
  (:documentation "Sets the blue `B` component of color `COLOR`."))
(defgeneric (setf a) (value color)
  (:documentation "Sets the alpha `A` component of color `COLOR`."))

(defgeneric map-color (color &optional surface)
  (:documentation "Maps the color `COLOR` to the pixel format of the surface `SURFACE` and returns 
the pixel value that best approximates the color value of the 
surface `SURFACE`. If the surface has a palette \(8-bit\) the index of the 
closest matching color in the palette will be returned. If the surface has an 
alpha component it will be returned as all `1` bits \(fully opaque\). If the surface 
color depth is less than 32-bpp then the unused upper bits of the return value can safely be ignored 
\(e.g., with a 16-bpp format the return value can be assigned to a Uint16, and similarly a Uint8 for an 8-bpp format\)."))

;; set-'s and get-'s
(defgeneric color-* (color)
  (:documentation "Returns the RGB/A components of color as a spread. 
If `COLOR` contains an alpha component then `RESULT` is `\(VALUES R G B A\)`
If `COLOR` contains no alpha component then `RESULT` is `\(VALUES R G B\)`"))
(defgeneric set-color (dst src)
  (:documentation "Copies the RGB/A color components to teh destination color `DST` from the source color `SRC`."))
(defgeneric set-color-* (color &key r g b a)
  (:documentation "Sets the red `R`, green `G`, blue `B` and alpha `A` components of the color `COLOR`.
`R`, `G`, `B` and `A` are `KEY`word parameters having default values of `0` if unspecified."))

(defgeneric point-* (point)
    (:documentation "Returns the `X` and `Y` coordinates of the object as a spread. 
The `RESULT` is `\(VALUES X Y\)`"))
(defgeneric set-point (dst src)
  (:documentation "Copies the `X` and `Y` coordinates to the destination `DST` from the source `SRC`."))
(defgeneric set-point-* (obj &key x y)
  (:documentation "Sets the `X` and `Y` coordinates of the object `OBJ`. `X` and `Y` are `KEY`word parameters."))

(defgeneric get-point (object)
  (:documentation "Returns the `X` and `Y` coordinates of object `OBJ` as a `POINT`."))

(defgeneric position-* (obj)
  (:documentation "See [POINT-*](#point-*)"))
(defgeneric set-position (dst src)
  (:documentation "See [SET-POINT](#set-point)"))
(defgeneric set-position-* (obj &key x y)
  (:documentation "See [SET-POINT-*](#set-point-*)"))

(defgeneric get-position (object)
  (:documentation "See [GET-POINT](#get-POINT)"))

(defgeneric rectangle-* (obj)
  (:documentation "Returns the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates of the object as a spread. 
The `RESULT` is `\(VALUES X Y WIDTH HEIGHT\)`"))
(defgeneric set-rectangle (dst src)
  (:documentation "Copies the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates to the destination rectangle `DST` from the source rectangle `SRC`."))
(defgeneric set-rectangle-* (rectangle &key x y w h)
  (:documentation "Sets the `X`, `Y`, `WIDTH` and `HEIGHT` coordinates of the rectangle `RECTANGLE`.
`X`, `Y`, `WIDTH` and `HEIGHT` are `KEY`word parameters having default values of `0` if unspecified."))

(defgeneric get-rectangle (obj)
  (:documentation "Returns the rectangle `RECTANGLE`."))
(defgeneric get-rectangle-* (obj)
  (:documentation "Creates and returns a `RECTANGLE` object from the `X`, `Y`, `WIDTH` and `HEIGHT` values in obj."))

(defgeneric set-surface (surface position)
  (:documentation "Sets the coordinates of the surface SURFACE to `POSITION`, 
where position is of type `POINT`."))
(defgeneric set-surface-* (surface &key x y)
  (:documentation "Sets the coordinates of the surface `SURFACE`.
`X` and `Y` are `KEY`word parameters having default values of `0` if unspecified."))

;; end set-'s and get-'s


(defgeneric width (obj)
  (:documentation "Returns the width of the object."))

(defgeneric height (obj)
  (:documentation "Returns the height of the object."))

(defgeneric x (obj)
  (:documentation "Returns the x coordinate of the object."))

(defgeneric y (obj)
  (:documentation "Returns the y coordinate of the object."))

(defgeneric x2 (obj)
  (:documentation "Returns `\(+ X WIDTH\)` of the object."))
(defgeneric y2 (obj)
  (:documentation "Returns `\(+ Y HEIGHT\)` of the object."))


(defgeneric (setf width) (value obj)
  (:documentation "Sets the width of the object."))
(defgeneric (setf height) (value obj)
  (:documentation "Sets the height of the object."))

(defgeneric (setf x) (value obj)
  (:documentation "Sets the X coordinate of the object."))
(defgeneric (setf y) (value obj)
  (:documentation "Sets the Y coordinate of the object."))

(defgeneric (setf x2) (value obj)
  (:documentation "Sets the WIDTH of the object to `\(- X2 X\)`"))
(defgeneric (setf y2) (value obj)
  (:documentation "Sets the HEIGHT of the object to `\(- Y2 Y\)`"))

(defgeneric free-color (color)
  (:documentation "Free's resources allocated to COLOR.
Meant specifically for the SDL_Color foreign object. 
`COLOR` and `COLOR-A` are ignored."))

(defgeneric free-surface (surface)
  (:documentation "Free's the resources allocated to `SURFACE`.
Specifically free's the wrapped SDL_Surface foreign object."))

(defgeneric free-rectangle (rectangle)
  (:documentation "Free's the resources allocated to `RECTANGLE`.
Specifically free's the wrapped SDL_Rect foreign object."))

(defgeneric free-rwops (rwops)
  (:documentation "Free's the resources allocated to `RWOPS`.
Specifically free's the wrapped SDL_rwops foreign object."))

(defgeneric free-font (font)
  (:documentation "Free's the resources allocated to the `FONT`."))

(defgeneric free-cached-surface (font)
  (:documentation "Frees resources allocated to the cached surface `SURFACE` in `FONT`, if any. 
Sets the `FONT`s `CACHED-SURFACE` slot to NIL."))

(defgeneric draw-font (&key font surface)
  (:documentation "Blit the cached surface in the font `FONT` to 
the destination surface `SURFACE`. The cached surface is created during a previous call to any of 
the DRAW-STRING* functions. Uses the `POSITION` of the cached surface to render at X/Y coordinates 
on the destination `SURFACE`. This function can provide a speed increase upon redraws when the text in `FONT` 
remains unchanged between screen updates."))

(defgeneric draw-font-at (position &key font surface)
  (:documentation "See [DRAW-FONT](#draw-font). 

##### Parameters

* `POINT` is the `X` and `Y` coordinates of the the `FONT`s cached surface, of type `POINT`."))

(defgeneric draw-font-at-* (x y &key font surface)
  (:documentation "See [DRAW-FONT](#draw-font) 

##### Parameters

* `X` and `Y` are the `INTEGER` position coordinates of the `FONT`s cached surface."))

(defgeneric color= (color1 color2)
  (:documentation "Returns `T` if colors match, returns `NIL` otherwise."))

(defgeneric any-color-but-this (color)
  (:documentation "Returns a new color that is different to the color `COLOR`."))

(defgeneric load-image (source &key color-key alpha image-type force free-rwops color-key-at)
  (:documentation
   "Creates and returns a new `SURFACE` from the image in `SOURCE`, or returns `NIL` if `SOURCE` does not contain a valid image
or the image type cannot be determined.

The *magic number* if present is be used to determine the image type. To load an image when the 
*magic number* is unavailable \(image formats such as `TGA` do not contain a *magic number*\), 
specify the image type using `:IMAGE-TYPE`.  All non-magicable image formats, such as `TGA`, 
must be specified using `IMAGE-TYPE`. To load a TGA image use `:IMAGE-TYPE :TGA` 

* `LISPBUILDER-SDL` only supports `BMP` images. Any alpha channel present in the source image is ignored. The new `SURFACE` is
created as an `RGB` surface, not `RGBA`.
* `LISPBUILDER-SDL-IMAGE` supports the following image formats, `BMP`, `GIF`, `JPG`, `LBM`, `PCX`, `PNG`, 
`PNM`, `TIF`, `XCF`, `XPM`, `XV`. `BMP` and `TGA`. Alpha channels are supported. The new `SURFACE` is created as `RGB` or `RGBA`
as appropriate.

##### Parameters

* `SOURCE` is an image.
* `COLOR-KEY` sets the color key, and turns on color keying.
* 'COLOR-KEY-AT' uses the pixel color at `POINT` x/y as the color key, and turns on color keying.
* `ALPHA` sets the transparency level for the surface, and turns on alpha blending. Must be in the range 0-255, where 255 is opaque and 0 is transparent.
* `IMAGE-TYPE` specifies the image type. May be `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TGA`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 
 The image type is determined using the *magic number* when present in the image if `NIL`.
 If the *magic number* is available and does not match `IMAGE-TYPE`, then `IMAGE-TYPE` is ignored.
* `FORCE` when `T` will ignore any *magic number* present in the image and load an image as `:IMAGE-TYPE`. 
 Images of type `TGA` must be loaded using `:FORCE T`.
* `FREE-RWOPS` will free a RWOPS passed as a parameter in `SOURCE`. Default is `T`

_NOTES_: The `:IMAGE-TYPE` and `:FORCE` keywords are ignored for `LISPBUILDER-SDL`.

##### Example

* To load a `BMP` image using the *magic number*

    \(LOAD-IMAGE \"image.bmp\"\)
    
* To load a `TGA` image

    \(LOAD-IMAGE \"image.tga\" :IMAGE-TYPE :TGA\)
    
* Try to load a `BMP` image as `TGA`

    \(LOAD-IMAGE \"image.bmp\" :IMAGE-TYPE :BMP :FORCE T\)"))