;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl)


(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color sdl:color) (bg-color sdl:color) justify (surface sdl:sdl-surface) (font ttf-font))
  (sdl:with-surface (font-surface (_render-string-shaded_ string fg-color bg-color font nil nil) t)
    (sdl:set-surface-* font-surface :x x :y y)
    (sdl:blit-surface font-surface surface))
  surface)

(defmethod _render-string-shaded_ ((string string) (fg-color sdl:color) (bg-color sdl:color) (font ttf-font) free cache)
  (let ((surf
	 (sdl:with-foreign-color-copy (fg-struct fg-color)
	   (sdl:with-foreign-color-copy (bg-struct bg-color)
	     (make-instance 'sdl:surface :fp (sdl-ttf-cffi::ttf-Render-UTF8-shaded (sdl:fp font) string fg-struct bg-struct))))))
    (when cache
      (setf (sdl:cached-surface font) surf))
    surf))