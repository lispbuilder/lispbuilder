
(in-package #:sdl-mixer)


(defmethod load-mus (filename path)
  "load in the supplied filename, must be an mp3, wav or ogg file"
  (let ((file (namestring (merge-pathnames filename path))))
    (if (and (stringp file) (probe-file file))
	(let ((music-fp (sdl-mixer-cffi::Load-MUS file)))
	  (if (sdl-base:is-valid-ptr music-fp)
	      (make-instance 'sdl-mixer-cffi::music :fp music-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))

(defmethod load-wav (filename path)
  "load in the supplied filename, must be an mp3, wav or ogg file"
  (let ((file (namestring (merge-pathnames filename path))))
    (if (and (stringp file) (probe-file file))
	(let ((chunk-fp (sdl-mixer-cffi::Load-wav file)))
	  (if (sdl-base:is-valid-ptr chunk-fp)
	      (make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))

(defmethod get-chunk ((channel integer))
  (let ((chunk-fp (sdl-mixer-cffi::get-chunk channel)))
    (if (sdl-base:is-valid-ptr chunk-fp)
	(make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp)
	nil)))

