;; This file contains some useful functions for using SDL_ttf from Common lisp
;; 2006 (c) Rune Nesheim, see LICENCE.

(in-package #:lispbuilder-sdl-ttf)

(defmacro with-init (&body body)
  `(unwind-protect
     (progn 
       (TTF_init)
       ,@body)
     (TTF_Quit)))


;defmacro with-open-font
; I sugest a syntax like this, with variable capture of font:
;    (with-open-font ("foo.ttf" 12)
;      (let ((surf (make-text-surface font "There's good intensions
;                                           and there's good deeds,
;	    	                            and they're as far apart 
;                                           as heaven and hell"
;		                      255 255 255)))
;        (bar surf)))
;
; But each time I tried to write it I did something wrong.
; I'm stupid. -RN


(defun open-font (filename size)
  "You must close the font to free it"
  (let ((font (TTF_OpenFont filename size)))
    (if (cffi:null-pointer-p font)
      (error (concatenate 'string "Failed to open font in location: " filename))
      font)))

(defun close-font (font)
  (TTF_CloseFont font))

(defun make-text-surface (font text r g b)
  (let ((fg-color (sdl:get-sdlcolor r g b)))
    (unwind-protect
      (TTF_RenderText_Solid font text fg-color)
    (cffi:foreign-free fg-color))))

