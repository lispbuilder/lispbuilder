;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric r (color))
(defgeneric g (color))
(defgeneric b (color))
(defgeneric a (color))

(defgeneric (setf r) (value color)
  (:documentation "Sets/Returns the red color component of the object."))
(defgeneric (setf g) (value color)
  (:documentation "Sets/Returns the green color component of the object."))
(defgeneric (setf b) (value color)
  (:documentation "Sets/Returns the blue color component of the object."))
(defgeneric (setf a) (value color)
  (:documentation "Sets/Returns the alpha color component of the object."))

(defgeneric pack-color (color)
  (:documentation "Packs [COLOR](#color) or [COLOR-A](#color-a) into a four byte `INTEGER`."))
(defgeneric pack-color-* (r g b &optional a)
  (:documentation "Packs [COLOR](#color) or [COLOR-A](#color-a) into a four byte `INTEGER`.
Assumes an alpha component when `A` is not `NIL`."))

(defgeneric map-color (color &optional surface)
  (:documentation
  "Maps [COLOR](#color) or [COLOR-A](#color-a) to the pixel format of [SURFACE](#surface) and returns 
the pixel value that best approximates the color value of the surface. 
If the surface has a palette \(8-bit\) the index of the 
closest matching color in the palette will be returned. If the surface has an 
alpha component it will be returned as all `1` bits \(fully opaque\). If the surface 
color depth is less than 32-bpp then the unused upper bits of the return value can safely be ignored 
\(e.g., with a 16-bpp format the return value can be assigned to a Uint16, and similarly a Uint8 for an 8-bpp format\)."))

(defgeneric map-color-* (r g b a &optional surface)
  (:documentation
  "Maps the color specified by the `R`, `G`, `B`, and `A` color components to the pixel format of [SURFACE](#surface) and returns 
the pixel value that best approximates the color value of the surface. 
If `A` is not `NIL` then the color is assumed to contain an alpha component.
See [MAP-COLOR](#map-color) for more details."))

(defgeneric color-* (color)
  (:documentation "Returns the `RGB/A` color components as a spread. 
[COLOR-A](#color-a) returns `\(VALUES R G B A\)`. 
[COLOR](#color) returns `\(VALUES R G B\)`"))
(defgeneric set-color (dst src)
  (:documentation "Copies the `RGB/A` color components to the destination `DST` from the source `SRC`."))
(defgeneric set-color-* (color &key r g b a)
  (:documentation "Sets `COLOR` to the red `R`, green `G`, blue `B` and alpha `A` color components."))

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


(defgeneric width (obj))
(defgeneric height (obj))
(defgeneric x (obj))
(defgeneric y (obj))
(defgeneric x2 (obj))
(defgeneric y2 (obj))

(defgeneric (setf width) (value obj)
  (:documentation "Sets/Returns the width of the object, as an `INTEGER`."))
(defgeneric (setf height) (value obj)
  (:documentation "Sets/Returns the height of the object, as an `INTEGER`."))

(defgeneric (setf x) (value obj)
  (:documentation "Sets/Returns the `X` coordinate of the object, as an `INTEGER`."))
(defgeneric (setf y) (value obj)
  (:documentation "Sets/Returns the `Y` coordinate of the object, as an `INTEGER`."))

(defgeneric (setf x2) (value obj)
  (:documentation "Returns `\(+ X WIDTH\)` of the object, as an `INTEGER`.
Sets the WIDTH of the object to `\(- X2 X\)`"))
(defgeneric (setf y2) (value obj)
  (:documentation "Returns `\(+ Y HEIGHT\)` of the object, as an `INTEGER`.
Sets the HEIGHT of the object to `\(- Y2 Y\)`"))

(defgeneric draw-font (&key font surface)
  (:documentation "Blit the cached [SURFACE](#surface) in [font](#FONT) to 
the destination `SURFACE`. The cached surface is created during a previous call to any of 
the DRAW-STRING* functions.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"))

(defgeneric draw-font-at (position &key font surface)
  (:documentation "See [DRAW-FONT](#draw-font). The cached surface is rendered at `POSITION` [POINT](#point).

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"))

(defgeneric draw-font-at-* (x y &key font surface)
  (:documentation "See [DRAW-FONT](#draw-font).  The cached surface is rendered at poisition `X` and `Y`.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_, and _LISPBUILDER-SDL-TTF_"))

(defgeneric color= (color1 color2)
  (:documentation "Returns `T` if the `RGB`<->`RGB` or `RGBA`<->`RGBA` color components in `COLOR1` and `COLOR2` match. 
Returns `NIL` otherwise."))

(defgeneric any-color-but-this (color)
  (:documentation "Returns a new color that is different to `COLOR`."))

(defgeneric load-image (source &key color-key alpha image-type force free-rwops color-key-at)
  (:documentation
   "Creates and returns a new `SURFACE` from the image in `SOURCE`, or returns `NIL` if `SOURCE` does not contain a valid image
or the image type cannot be determined.

The *magic number* if present is be used to determine the image type. To load an image when the 
*magic number* is unavailable \(image formats such as `TGA` do not contain a *magic number*\), 
specify the image type using `:IMAGE-TYPE`.  All non-magicable image formats, such as `TGA`, 
must be specified using `IMAGE-TYPE`. To load a TGA image use `:IMAGE-TYPE :TGA` 

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

##### Example

* To load a `BMP` image using the *magic number*

    \(LOAD-IMAGE \"image.bmp\"\)
    
* To load a `TGA` image

    \(LOAD-IMAGE \"image.tga\" :IMAGE-TYPE :TGA\)
    
* Try to load a `BMP` image as `TGA`

    \(LOAD-IMAGE \"image.bmp\" :IMAGE-TYPE :BMP :FORCE T\)

##### Packages

* Also supported in _LISPBUILDER-SDL-IMAGE_
* _LISPBUILDER-SDL_ only supports `BMP` images. Any alpha channel present in the source image is ignored. The new `SURFACE` is
created as an `RGB` surface, not `RGBA`.
* _LISPBUILDER-SDL-IMAGE_ supports the following image formats, `BMP`, `GIF`, `JPG`, `LBM`, `PCX`, `PNG`, 
`PNM`, `TIF`, `XCF`, `XPM`, `XV`. `BMP` and `TGA`. Alpha channels are supported. The new `SURFACE` is created as `RGB` or `RGBA`
as appropriate.
* `:IMAGE-TYPE` and `:FORCE` are ignored for _LISPBUILDER-SDL_.
"))

(defgeneric image-p (source image-type)
  (:documentation
   "Returns `T` when the image type in `SOURCE` is of `IMAGE-TYPE`. Returns `NIL` otherwise. 
Attempts to detect the image type using the *magic number* contained in the image if one is available.
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.
 `IMAGE-TYPE` must be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, 
`:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 

##### Example

    \(RWOPS-P SOURCE :IMAGE-TYPE :BMP\)
    \(IMAGE-P \"image.bmp\" :IMAGE-TYPE :BMP\)

##### Packages

* Supported in _LISPBUILDER-SDL-IMAGE_"))

(defgeneric image-type-of (source)
  (:documentation
   "Returns the type of image in source `SOURCE`. 
Attempts to detect the image type using the *magic number* contained in the image if one is available. 
 Returns one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, 
`:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`, if the image type can be determined. 
Returns `NIL` if the image cannot be determined \(The *magic number* is not supported or the *magic number* is not found\).
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.

##### Example

    \(IMAGE-TYPE-OF SOURCE\)
    \(IMAGE-TYPE-OF \"image.bmp\"\)

##### Packages

* Supported in _LISPBUILDER-SDL-IMAGE_"))

(defgeneric alpha-enabled-p (surface))
(defgeneric (setf alpha-enabled-p) (value surface)
  (:documentation "Manage alpha blending for a `SURFACE`. 
Returns `T` when alpha blending is enabled for `SURFACE`, and `NIL` when disabled.
Enable surface alpha blending for when `T`. Disable surface alpha blending when `NIL`. 
A `SURFACE` need not have a pixel alpha component \(RGBA\) to use surface alpha blending."))

(defgeneric alpha (surface))
(defgeneric (setf alpha) (value surface)
  (:documentation "Manages per-surface alpha. 
Returns the per-surface alpha value. 0 is transparent, and 255 is opaque.
Sets the per-surface alpha value. 0 is transparent, and 255 is opaque. 

*Note*: The per-surface alpha value of 128 is considered a special case and is optimised, so it's much faster than other per-surface values.
*Note*: A surface need not have an alpha channel to use alpha blending.
*Note*: When blitting, the presence or absence of [SDL-SRC-ALPHA](#sdl-src-alpha) is relevant only on the source surface, not the destination. 
*Note*: Per-pixel and per-surface alpha cannot be combined; the per-pixel alpha is always used if available."))

(defgeneric color-key-enabled-p (surface))
(defgeneric (setf color-key-enabled-p) (value surface)
  (:documentation "Manages colorkeying for a `SURFACE`.
Returns `T` when color keying is enabled, and `NIL` when color keying is disabled.
Enables color keying when `T`. Disable color keying when `NIL`"))

(defgeneric color-key (surface))
(defgeneric (setf color-key) (value surface)
  (:documentation   "Manages the colorkey for a `SURFACE`.
Returns the current color key \(transparent pixel\) as [COLOR](#color).
Set the \(RGB\) [COLOR](#color) key \(transparent pixel\)."))

(defgeneric pixel-alpha-enabled-p (surface)
  (:documentation "Returns `T` if a pixel alpha component \(RGBA\) is available, or `NIL` if unavailable \(RGB\).
*Note*: The pixel alpha component differs from the surface alpha component which is 
retrieved using [ALPHA-ENABLED-P](#alpha-enabled-p)."))

(defgeneric rle-accel-enabled-p (surface))
(defgeneric (setf rle-accel-enabled-p) (value surface)
  (:documentation "Manages RLE acceleration for a `SURFACE`.
Returns `T` if RLE acceleration is enabled, and `NIL` when RLE is disabled.
Enables RLE blit acceleration when `T`, disables RLE acceleration when `NIL`. 
RLE acceleration can substantially speed up blitting of images with large horizontal runs 
of transparent pixels (i.e., pixels that match the key color)."))

(defgeneric clip-rect (surface))
(defgeneric (setf clip-rect) (value surface)
  (:documentation "Manages the clipping `RECTANGLE` for a`SURFACE`.
Returns the clipping `RECTANGLE` for `SURFACE`.
Sets the clipping [RECTANGLE](#rectangle) for the `SURFACE`. Removes the clipping rectangle when `NIL`.
When `SURFACE` is the destination of a blit, only the area within the clipping rectangle is 
drawn into."))

(defgeneric initialise-font (font-definition)
  (:documentation "Returns a new [SDL-BITMAP-FONT](#sdl-bitmap-font) initialized from `FONT-DEFINITION` data, or `NIL` 
if the font cannot be created. `FONT-DEFINITION` must be one of the following built-in fonts: 
`*FONT-10X20*`, `*FONT-5X7*`, `*FONT-5X8*`, `*FONT-6X10*`, `*FONT-6X12*`, `*FONT-6X13*`, 
`*FONT-6X13B*`, `*FONT-6X13O*`, `*FONT-6X9*`, `*FONT-7X13*`, `*FONT-7X13B*`, `*FONT-7X13O*`, 
`*FONT-7X14*`, `*FONT-7X14B*`, `*FONT-8X13*`, `*FONT-8X13B*`, `*FONT-8X13O*`, `*FONT-8X8*`, 
`*FONT-9X15*`, `*FONT-9X15B*`, `*FONT-9X18*` OR `*FONT-9X18B*`.

##### Packages

* Also supported in _LISPBUILDER-SDL-GFX_"))

(defgeneric set-default-font (font)
  (:documentation
   "Sets the font `FONT` as the default font to be used for subsequent
font rendering or drawing operations. Binds the symbol `\*DEFAULT-FONT\*` to font. 
Functions that take a `FONT` argument use `\*DEFAULT-FONT\*` unless
otherwise specified.
Returns a new `FONT`, or `NIL` if unsuccessful."))

(defgeneric _set-font-style_ (font style))
(defgeneric _get-glyph-metric_ (font ch metric))
(defgeneric _get-font-size_ (font text size))
(defgeneric _get-font-style_ (font))
(defgeneric _get-font-height_ (font))
(defgeneric _get-font-ascent_ (font))
(defgeneric _get-font-descent_ (font))
(defgeneric _get-font-line-skip_ (font))
(defgeneric _get-font-faces_ (font))
(defgeneric _is-font-face-fixed-width_ (font))
(defgeneric _get-font-face-family-name_ (font))
(defgeneric _get-font-face-style-name_ (font))

(defgeneric _render-string-solid_ (string font color free cache))
(defgeneric _draw-string-solid-*_ (string x y justify surface font color))

(defgeneric _render-string-shaded_ (string fg-color bg-color font free cache))
(defgeneric _draw-string-shaded-*_ (string x y fg-color bg-color justify surface font))

(defgeneric _render-string-blended_ (string font color free cache))
(defgeneric _draw-string-blended-*_ (string x y justify color surface font))

(defgeneric char-width (font))
(defgeneric char-height (font))
(defgeneric char-pitch (font))
(defgeneric char-size (font))
(defgeneric char-data (font))
(defgeneric font-data (font))

(defgeneric free-cached-surface (font))

