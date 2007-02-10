;;;; lispbuilder-sdl
;;;; The OO wrapper for the lispbuilder-sdl package
;;;; (C)2006 Luke Crook <luke@balooga.com>

(in-package #:lispbuilder-sdl)

(defgeneric fp (object)
  (:documentation "Returns the default foreign object for OBJECT."))

(defgeneric fp-position (object)
  (:documentation "Returns the default SDL_Rect foreign object for OBJECT."))

(defgeneric pack-color (color)
  (:documentation "Pack the RGBA components in color into a four byte INTEGER."))

(defgeneric r (color)
  (:documentation "Returns the red component of color as an INTEGER."))
(defgeneric g (color)
  (:documentation "Returns the green component of color as an INTEGER."))
(defgeneric b (color)
  (:documentation "Returns the blue component of color as an INTEGER."))
(defgeneric a (color)
  (:documentation "Returns the alpha component of color as an INTEGER."))

(defgeneric (setf r) (value color)
  (:documentation "Sets the red component of color."))
(defgeneric (setf g) (value color)
  (:documentation "Sets the green component of color."))
(defgeneric (setf b) (value color)
  (:documentation "Sets the blue component of color."))
(defgeneric (setf a) (value color)
  (:documentation "Sets the alpha component of color."))

(defgeneric map-color (color &optional surface)
  (:documentation "Maps the color COLOR to the pixel format of the surface SURFACE, and returns the 
pixel value as an INTEGER. 
If the format has a palette (8-bit) the index of the closest matching color in the palette will be returned.
If the specified pixel format has an alpha component it will be returned as all 1 bits (fully opaque).
RESULT is a pixel value that best approximates the given RGB color value for the surface SURFACE."))


;; set-'s and get-'s
(defgeneric color-* (color)
  (:documentation "Returns the separate RGB/A components of color using VALUES. 
If COLOR contains an alpha component then RESULT is \(VALUES R G B A\)
If COLOR does not contain an alpha component then RESULT is \(VALUES R G B)"))
(defgeneric set-color (dst src)
  (:documentation "Copies the RGB/A color components to DST from SRC."))
(defgeneric set-color-* (color &key r g b a)
  (:documentation "Sets the red R, green G, blue B and alpha A components of the color COLOR.
R, G, B, A are KEYword parameters having default values of 0."))

(defgeneric point-* (point)
    (:documentation "Returns the separate X and Y coordinates of the point POINT using VALUES. 
The RESULT is \(VALUES X Y\)"))
(defgeneric set-point (dst src)
  (:documentation "Copies the X and Y coordinates to DST from SRC."))
(defgeneric set-point-* (obj &key x y)
  (:documentation "Sets the X and Y coordinates of the point POINT.
X and Y are KEYword parameters having default values of 0."))


(defgeneric position-* (obj)
  (:documentation "See POINT-*"))
(defgeneric set-position (dst src)
  (:documentation "See SET-POINT"))
(defgeneric set-position-* (obj &key x y)
  (:documentation "See SET-POINT-*"))

(defgeneric rectangle-* (rectangle)
  (:documentation "Returns the separate X, Y, WIDTH and HEIGHT coordinates of the rectangle RECTANGLE using VALUES. 
The RESULT is \(VALUES X Y WIDTH HEIGHT\)"))
(defgeneric set-rectangle (dst src)
  (:documentation "Copies the X, Y, WIDTH and HEIGHT coordinates to the rectangle DST from the rectangle SRC."))
(defgeneric set-rectangle-* (rectangle &key x y w h)
  (:documentation "Sets the X, Y, WIDTH and HEIGHT coordinates of the rectangle RECTANGLE.
X, Y, WIDTH and HEIGHT are KEYword parameters having default values of 0."))

(defgeneric set-surface (surface position)
  (:documentation "Sets the X and Y coordinates of the surface SURFACE from position, 
where position is of type POINT."))
(defgeneric set-surface-* (surface &key x y)
  (:documentation "Sets the X and Y coordinates of the surface SURFACE.
X and Y are KEYword parameters having default values of 0."))

;; end set-'s and get-'s


(defgeneric width (obj)
  (:documentation "Returns the width of the SURFACE or RECTANGLE as an INTEGER."))

(defgeneric height (obj)
  (:documentation "Returns the height of the SURFACE or RECTANGLE as an INTEGER."))

(defgeneric x (obj)
  (:documentation "Returns the x coordinate of the object OBJ."))

(defgeneric y (obj)
  (:documentation "Returns the y coordinate of the object OBJ."))

(defgeneric x2 (obj)
  (:documentation "Returns \(+ X WIDTH\) of the object OBJ as an INTEGER."))
(defgeneric y2 (obj)
  (:documentation "Returns \(+ Y HEIGHT\) of the object OBJ as an INTEGER."))


(defgeneric (setf width) (value obj)
  (:documentation "Sets the width of the object, where WIDTH is an INTEGER."))
(defgeneric (setf height) (value obj)
  (:documentation "Sets the height of the object, where HEIGHT is an INTEGER."))

(defgeneric (setf x) (value obj)
  (:documentation "Sets the X coordinate of the object, where X is an INTEGER."))
(defgeneric (setf y) (value obj)
  (:documentation "Sets the Y coordinate of the object, where Y is an INTEGER."))

(defgeneric (setf x2) (value obj)
  (:documentation "Sets the WIDTH of the object to \(- X2 X)"))
(defgeneric (setf y2) (value obj)
  (:documentation "Sets the HEIGHT of the object to \(- Y2 Y)"))

(defgeneric free-color (color)
  (:documentation "Free's the resources associated with COLOR.
Meant specifically for the foreign object SDL_Color. Otherwise COLOR and COLOR-A are ignored."))

(defgeneric free-surface (surface)
  (:documentation "Free's the resources associated with SURFACE.
Specifically free's the wrapped foreign SDL_Surface."))

(defgeneric free-rectangle (rectangle)
  (:documentation "Free's the resources associated with RECTANGLE.
Specifically free's the wrapped foreign SDL_Rect."))

(defgeneric free-rwops (rwops)
  (:documentation "Free's the resources associated with RWOPS.
Specifically free's the wrapped foreign SDL_rwops."))

(defgeneric free-font (font)
  (:documentation "Free's the resources associated with the FONT."))

(defgeneric free-cached-surface (font)
  (:documentation "Frees the cached surface in FONT, if any. 
Sets the CACHED-SURFACE slot to NIL."))

(defgeneric draw-font (&key font surface)
  (:documentation "Blit the cached SURFACE in FONT to the destination surface SURFACE. 
The cached surface is created during a previous call to any of the DRAW-STRING* functions. 
Uses POSITION in the cached SURFACE to render to the X/Y coordinates on the destination SURFACE. 

This function can speed up blitting when the text remains unchanged between screen updates."))

(defgeneric draw-font-at (position &key font surface)
  (:documentation "See DRAW-FONT. 
POINT is used to position the cached SURFACE, where POINT is of type SDL:POINT."))

(defgeneric draw-font-at-* (x y &key font surface)
  (:documentation "See DRAW-FONT. 
X and Y are used to position the cached SURFACE, where X and Y are INTEGERS."))
