
(in-package #:lispbuilder-sdl)


(defun load-image (filename path &key key-color alpha-value)
  (let ((surf (surface (sdl-base::load-image filename path))))
    (when surf
      (when key-color (set-color-key key-color :surface surf))
      (when alpha-value (set-alpha alpha-value :surface surf)))
    surf))

