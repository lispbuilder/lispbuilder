;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl)


(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color color) (bg-color color) justify (surface sdl-surface) (font ttf-font))
  (with-surface (font-surface (_render-string-shaded_ string fg-color bg-color font nil nil) t)
    (set-surface-* font-surface :x x :y y)
    (blit-surface font-surface surface))
  surface)

#+lispbuilder-sdl-ttf-glue
(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (let ((surf
	 (with-foreign-color-copy (fg-struct fg-color)
	   (with-foreign-color-copy (bg-struct bg-color)
	     (make-instance 'surface :fp (sdl-ttf-cffi::ttf-Render-UTF8-shaded (fp font) string fg-struct bg-struct))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

#-lispbuilder-sdl-ttf-glue
(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (let ((surf (make-instance 'surface
                             :fp (sdl-ttf-cffi::ttf-Render-UTF8-shaded (fp font) string
                                                                       (+ (ash (b fg-color) 16)
                                                                          (ash (g fg-color) 8)
                                                                          (r fg-color))
                                                                       (+ (ash (b bg-color) 16)
                                                                          (ash (g bg-color) 8)
                                                                          (r bg-color))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))
