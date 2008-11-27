;; SDL (Simple Media Layer) library using CFFI for foreign function interfacing...
;; (C)2006 Justin Heyes-Jones <justinhj@gmail.com> and Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl)

;;;; Globals

(defvar *default-surface* nil
  "Functions that accept the `KEY`word parameter `:SURFACE` will most likely bind to the 
symbol `\*DEFAULT-SURFACE*\` by default if `SURFACE` is not specified. 

A surface is bound to `\*DEFAULT-SURFACE*\` by the following macros: [WITH-SURFACE](#with-surface), 
and [WITH-SURFACES](#with-surfaces).

##### Example

    \(DRAW-SURFACE SURF-1 :SURFACE SDL:*DEFAULT-DISPLAY*\)
    \(DRAW-SURFACE SURF-2 :SURFACE SDL:*DEFAULT-DISPLAY*\)
    \(DRAW-SURFACE SURF-2 :SURFACE SDL:*DEFAULT-DISPLAY*\)

The above can be shortened using by setting the `\*DEFAULT-SURFACE\*` to the display surface.

    \(WITH-SURFACE \(DISP SDL:*DEFAULT-DISPLAY*\)
      \(DRAW-SURFACE SURF-1\)
      \(DRAW-SURFACE SURF-2\)
      \(DRAW-SURFACE SURF-2\)\)")

(defvar *default-display* nil
  "The symbol `\*DEFAULT-DISPLAY\*` is bound to the current display surface 
[DISPLAY-SURFACE](#display-surface)) by the function [WINDOW](#WINDOW)).")
  
(defvar *default-color* nil
    "Functions that accept the `KEY`word parameter `COLOR` will most likely bind to the 
symbol `\*DEFAULT-COLOR*\` by default if `COLOR` is not specified. 

A color is bound to `\*DEFAULT-COLOR*\` by the following macro: [WITH-COLOR](#with-color).

##### Example

    \(DRAW-BOX A-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)
    \(DRAW-BOX B-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)
    \(DRAW-BOX C-BOX :SURFACE SDL:DEFAULT-DISPLAY* :COLOR SDL:*BLACK*\)

The above can be shortened by setting `\*DEFAULT-COLOR\*` to `\*BLACK\*`.

    \(WITH-SURFACE \(DISP SDL:*DEFAULT-DISPLAY*\)
      \(WITH-COLOR \(COL SDL:*BLACK*\)
        \(DRAW-BOX A-BOX\)
        \(DRAW-BOX B-BOX\)
        \(DRAW-BOX C-BOX\)\)\)")

(defvar *opengl-context* nil
  "The symbol `\*OPENGL-CONTEXT\*` is `T` when an OpenGL display context is created, and `NIL` otherwise.
[UPDATE-SURFACE](#update-surface) will swap the `OPENGL` buffers when `\*OPENGL-CONTEXT\*` is `T`, 
and swap the `SDL` video buffers otherwise.")

(defvar *default-font* nil
  "Functions that accept the `KEY`word parameter `FONT` will most likely bind to the 
symbol `\*DEFAULT-FONT*\` by default if `FONT` is not specified. 

A font is bound to `\*DEFAULT-FONT*\` by the following; [WITH-DEFAULT-FONT](#with-default-font), 
[WITH-FONT](#with-font) and [INITIALISE-DEFAULT-FONT](#initialise-default-font).

##### Example

    \(draw-string-solid-* \"draw string centered\" 100 100
                          :justify :center :color sdl:*white* :font a-font\)
    \(draw-string-solid-* \"draw string left\" 100 100
                          :justify :left :color sdl:*white* :font a-font\)
    \(draw-string-solid-* \"draw string right\" 100 100
                          :justify :right :color sdl:*white* :font a-font\)

The above can be shortened by setting `\*DEFAULT-FONT\*` to `a-font`.

    \(WITH-DEFAULT-FONT \(a-font\)
      \(WITH-COLOR \(COL SDL:*WHITE*\)
        \(DRAW-STRING-SOLID-* \"draw string centered\" 100 100 :JUSTIFY :CENTER\)
        \(DRAW-STRING-SOLID-* \"draw string left\" 100 100 :JUSTIFY :LEFT\)
        \(DRAW-STRING-SOLID-* \"draw string right\" 100 100 :JUSTIFY :RIGHT\)\)\)")
(defvar *default-position* nil)
(defvar *default-rectangle* nil)


;(defvar *font-path* (merge-pathnames "font.bmp" (or *load-truename* *default-pathname-defaults*)))
(defvar *default-font-path* (make-pathname :host (pathname-host #.(or *compile-file-truename*
								      *load-truename*))
					   :directory (pathname-directory #.(or *compile-file-truename*
										*load-truename*)))
  "The path to any fonts used in teh `LISPBUILDER-SDL` packages.")
;; (defvar *renderer* nil)
;; (defvar *quit* nil)

(defvar *sdl-initialized* nil)
(defvar *sdl-init-on-startup* nil)
(defvar *sdl-quit-on-exit* nil)
(defvar *initialize-on-startup* (logior SDL-INIT-VIDEO))
(defvar *quit-on-exit* (logior SDL-INIT-VIDEO))

(defvar *external-init-on-startup* nil
  "The list of functions that are called from [INIT-SDL](#init-sdl).")
(defvar *external-quit-on-exit* nil
  "The list of functions that are called from [QUIT-SDL](#quit-sdl).")

;; (declaim (INLINE renderer))
;; (defun renderer ()
;;   *renderer*)
;; (defsetf renderer set-renderer)
;; (defun set-renderer (renderer)
;;   (setf *renderer* renderer))
