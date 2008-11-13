;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric r (color)
  (:documentation "Returns the red color component of the object."))
(defgeneric g (color)
  (:documentation "Returns the green color component the object."))
(defgeneric b (color)
  (:documentation "Returns the blue color component of the object."))
(defgeneric a (color)
  (:documentation "Returns the alpha color component of the object."))

(defgeneric (setf r) (value color)
  (:documentation "Sets the red color component of the object."))
(defgeneric (setf g) (value color)
  (:documentation "Sets the green color component of the object."))
(defgeneric (setf b) (value color)
  (:documentation "Sets the blue color component of the object."))
(defgeneric (setf a) (value color)
  (:documentation "Sets the alpha color component of the object."))

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


(defgeneric width (obj)
  (:documentation "Returns the width of the object, as an `INTEGER`. 
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))

(defgeneric height (obj)
  (:documentation "Returns the height of the object, as an `INTEGER`.
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))

(defgeneric x (obj)
  (:documentation "Returns the `X` coordinate of the object, as an `INTEGER`.
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))

(defgeneric y (obj)
  (:documentation "Returns the `Y` coordinate of the object, as an `INTEGER`.
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))

(defgeneric x2 (obj)
  (:documentation "Returns `\(+ X WIDTH\)` of the object, as an `INTEGER`."))
(defgeneric y2 (obj)
  (:documentation "Returns `\(+ Y HEIGHT\)` of the object, as an `INTEGER`."))


(defgeneric (setf width) (value obj)
  (:documentation "Sets the width of the object."))
(defgeneric (setf height) (value obj)
  (:documentation "Sets the height of the object."))

(defgeneric (setf x) (value obj)
  (:documentation "Sets the `X` `INTEGER` coordinate of the object.
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))
(defgeneric (setf y) (value obj)
  (:documentation "Sets the `Y` `INTEGER` coordinate of the object.
When `OBJ` is a [FONT](#font), use the cached [SURFACE](#surface)."))

(defgeneric (setf x2) (value obj)
  (:documentation "Sets the WIDTH of the object to `\(- X2 X\)`"))
(defgeneric (setf y2) (value obj)
  (:documentation "Sets the HEIGHT of the object to `\(- Y2 Y\)`"))

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
