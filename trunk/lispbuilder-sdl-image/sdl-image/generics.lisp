
(in-package #:lispbuilder-sdl)

(defgeneric image-p (source image-type)
  (:documentation
   "Returns `T` if the source `SOURCE` contains an image of type `IMAGE-TYPE`. Returns `NIL` otherwise. 
Attempts to detect the image type using the *magic number* contained in the image if one is available.
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*.

##### Example

    \(RWOPS-P SOURCE :IMAGE-TYPE :BMP\)"))

(defgeneric image-type-of (source)
  (:documentation
   "Returns the type of image in source `SOURCE`. 
Attempts to detect the image type using the *magic number* contained in the image if one is available. 

##### Returns

* Returns the image type of `SOURCE` which may be one of 
`:BMP`, `:GIF`, `:JPG`, `:LBM`, `:PCX`, `:PNG`, `:PNM`, `:TIF`, `:XCF`, `:XPM` or `:XV`, if the image type can be determined. 
Returns `NIL` if the image cannot be determined \(The *magic number* is not supported or the *magic number* is not found\).
 `NIL` is always returned for images of type `TGA` as a `TGA` image does not contain a *magic number*. "))

;; (defgeneric load-image (source &key key-color alpha-value image-type force free)
;;   (:documentation
;;    "Creates and returns a new surface from the source `SOURCE`.

;; Unless `:FORCE T`, the *magic number* will be used to determine the image type contained in the `SDL:RWOPS` in `SOURCE`. 
;; To load an image when the *magic number* is unavailable \(image formats such as `TGA` do not contain a *magic number*\), 
;; specify the image type using the `KEY`word `IMAGE-TYPE`. If the *magic number* is available 
;; and does not match `IMAGE-TYPE`, then `IMAGE-TYPE` is ignored. 
;; To load an image as `IMAGE-TYPE` when the *magic number* is available \(effectively ignoring the *magic number*\), 
;; specify `:FORCE T`. It is probably best to avoid using `:FORCE T` unless you know what you are doing.

;; ##### Returns

;; * Returns a new `SDL:SURFACE`, or `NIL` if `SOURCE` does not contain a valid image or the image type cannot be determined. 
;;  All non-magicable image formats, such as `TGA`, must be specified using `iMAGE-TYPE`. 
;; To load a TGA image, for example, use `:IMAGE-TYPE :TGA` 

;; ##### Example

;; * To load a `BMP` image using the *magic number* 

;;     \(CREATE-IMAGE-FROM-RWOPS SOURCE\)

;; * To load a `TGA` image

;;     \(CREATE-IMAGE-FROM-RWOPS SOURCE :IMAGE-TYPE :TGA\)

;; * To load a `BMP` image as `TGA`

;;     \(CREATE-IMAGE-FROM-RWOPS SOURCE :IMAGE-TYPE :TGA :FORCE T\)"))
