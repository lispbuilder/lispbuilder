
(in-package #:lispbuilder-sdl-image)

(defgeneric create-image-from-RWops (source &key image-type force free)
  (:documentation "Creates a new SDL:SURFACE from the image contained in SOURCE.
The image 'magic number' contained in the image is used to detect the image type and automatically load the image. 

SOURCE is of type sdl:RWOPS

Retuns a new SURFACE, or NIL if SOURCE does not contain a valid image, or the image type cannot be determined.

Will free the sdl:RWOPS if :FREE is T. 

To load an image as a specific image type, set the :IMAGE-TYPE to the desired type. 
The image type can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TGA, :TIF, :XCF, :XPM or :XV. 
If the 'magic number' is available and does not match :IMAGE-TYPE, then :IMAGE-TYPE is ignored.

Use :FORCE T to override the 'magic number' when attempting to load an image as a different type.

All 'non-magicable' image formats, such as TGA, must be specified using :iMAGE-TYPE or :FORCE and IMAGE-TYPE. 
For example, to load a TGA image use :IMAGE-TYPE :TGA"))

(defgeneric rwops-type-of (source)
  (:documentation "Returns the image type of SOURCE. Where SOURCE is of the type RWOPS.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

Returns one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV, or NIL if the image type 
cannot be determined or if there is no magic number available. 

Note: This means that NIL is always returned for images of type TGA."))

(defgeneric rwops-p (source image-type)
  (:documentation "Returns T if an image SOURCE is of the specified type IMAGE-TYPE, returns NIL if otherwise.
Attempts to detect the image type using the 'magic number' contained in the image, if one is available. 

SOURCE is of type RWOPS

:IMAGE-TYPE can be one of :BMP, :GIF, :JPG, :LBM, :PCX, :PNG, :PNM, :TIF, :XCF, :XPM or :XV. 

Note: NIL is always returned for images of type TGA, as a TGA image does not contain a 'magic number'."))


