
(in-package #:sdl-mixer)


(defmethod mix-load-mus (filename path)
  "load in the supplied filename, must be an mp3, wav or ogg file"
  (let ((file (namestring (merge-pathnames filename path))))
    (if (and (stringp file) (probe-file file))
	(let ((mix-music-fp (sdl-mixer-cffi::Mix-Load-MUS file)))
	  (if (sdl-base:is-valid-ptr mix-music-fp)
	      (make-instance 'sdl-mixer-cffi::mix-music :fp mix-music-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))




