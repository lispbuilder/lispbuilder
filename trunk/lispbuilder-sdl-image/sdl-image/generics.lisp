
(in-package #:lispbuilder-sdl)

(defgeneric image-p (source image-type)
  (:documentation
   "Returns `T` when the image type in `SOURCE` is of `IMAGE-TYPE`. Returns `NIL` otherwise. 
Attempts to detect the image type using the *magic number* contained in the image if one is available.
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.
 `IMAGE-TYPE` must be one of `:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, 
`:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`. 

##### Example

    \(RWOPS-P SOURCE :IMAGE-TYPE :BMP\)
    \(IMAGE-P \"image.bmp\" :IMAGE-TYPE :BMP\)"))

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
    \(IMAGE-TYPE-OF \"image.bmp\"\)"))

