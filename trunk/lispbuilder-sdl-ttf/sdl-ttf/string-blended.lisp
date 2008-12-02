;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl)

(defmethod _render-string-blended_ ((string string) (color sdl:color) (font ttf-font) free cache)
  (let ((surf 
	 (sdl:with-foreign-color-copy (col-struct color)
	   (make-instance 'sdl:surface
			  :fp (sdl-ttf-cffi::ttf-Render-UTF8-blended (sdl:fp font) string col-struct)))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))

(defmethod _draw-string-blended-*_ ((string string) (x integer) (y integer) justify (surface sdl:sdl-surface) (font ttf-font) (color sdl:color))
  (sdl:with-surface (font-surface (_render-string-blended_ string color font nil nil) t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  surface)

