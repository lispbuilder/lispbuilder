
(in-package #:lispbuilder-sdl)


(defmethod load-image ((filename string)
		       &key key-color alpha-value (image-type nil) (force nil) (free nil)
		       key-color-at)
  "Load a BMP image from a file at location `FILENAME`. Returns a new SURFACE.

##### Parameters

* `FILENAME` is the location of the image file on disk, as a `STRING`.
* `KEY-COLOR` when not `NIL` is the color to be used as the transpart pixel.
When `KEY-COLOR` is `NIL`, the surface is created without a key color. See
[SET-COLOR-KEY](#set-color-key) for more detailed information.
* `KEY-COLOR-AT` is the [POINT](#pixel) x/y coords of the pixel on the surface to be used as the key color.
is `NIL`, the surface is created without a key color. See
[SET-COLOR-KEY](#set-color-key) for more detailed information.
* `ALPHA` when between `0` and `255` will set the level of alpha transparency for the new surface.
When `ALPHA` is `NIL`, the new surface is created without alpha transparency. See
[SET-ALPHA](#set-alpha) for more detailed information."
  (declare (ignore image-type force free))
  (let ((surf (make-instance 'surface
			     :surface (sdl-base::load-image filename)
			     :key-color key-color
			     :alpha-value alpha-value)))
    (when key-color-at
      (set-color-key (read-pixel key-color-at :surface surf) :surface surf))
    surf))

(defun save-image (surface filename)
  "Saves the surface `SURFACE` as a BMP image to a file at location `FILENAME`."
  (check-type surface sdl-surface)
  (let ((file (namestring filename)))
    (sdl-cffi::SDL-Save-BMP-RW (fp surface) (sdl-cffi::SDL-RW-FROM-FILE file "wb") 1)))

