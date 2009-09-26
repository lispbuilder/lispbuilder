;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl)

(defmethod _draw-string-blended-*_ ((string string) (x integer) (y integer) justify (surface sdl-surface) (font ttf-font) (color color))
  (with-surface (font-surface (_render-string-blended_ string color font nil nil) t)
    (set-surface-* font-surface :x x :y y)
    (blit-surface font-surface surface))
  surface)

#+lispbuilder-sdl-ttf-glue
(defmethod _render-string-blended_ ((string string) (color color) (font ttf-font) free cache)
  (let ((surf 
	 (with-foreign-color-copy (col-struct color)
	   (make-instance 'surface
			  :fp (sdl-ttf-cffi::ttf-Render-UTF8-blended (fp font) string col-struct)))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

#-lispbuilder-sdl-ttf-glue
(defmethod _render-string-blended_ ((string string) (color color) (font ttf-font) free cache)
  (let ((surf (make-instance 'surface
                             :fp (sdl-ttf-cffi::ttf-Render-UTF8-blended (fp font) string (+ (ash (b color) 16)
                                                                                                (ash (g color) 8)
                                                                                                (r color))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))
