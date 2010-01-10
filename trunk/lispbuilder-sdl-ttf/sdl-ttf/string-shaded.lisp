;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.
;; 2007 (c) Luke Crook

(in-package #:lispbuilder-sdl)


(defmethod _draw-string-shaded-*_ ((string string) (x integer) (y integer) (fg-color color) (bg-color color) justify (surface sdl-surface) (font ttf-font))
  (with-surface (font-surface (_render-string-shaded_ string fg-color bg-color font nil nil) t)
    (set-surface-* font-surface :x x :y y)
    (blit-surface font-surface surface))
  surface)

(defmethod _render-string-shaded_ ((string string) (fg-color color) (bg-color color) (font ttf-font) free cache)
  (let ((surf nil))
    (with-foreign-color-copy (fg-struct fg-color)
      (with-foreign-color-copy (bg-struct bg-color)
        (multiple-value-bind (fg bg)
            (if (cffi:foreign-symbol-pointer "TTF_glue_RenderText_Shaded")
              (values fg-struct bg-struct)
              (values (+ (ash (b fg-color) 16)
                         (ash (g fg-color) 8)
                         (r fg-color))
                      (+ (ash (b bg-color) 16)
                         (ash (g bg-color) 8)
                         (r bg-color))))
          (setf surf (make-instance 'surface :fp (sdl-ttf-cffi::render-text-shaded (fp font) string fg bg))))))
    (when cache
      (setf (cached-surface font) surf))
    surf))

