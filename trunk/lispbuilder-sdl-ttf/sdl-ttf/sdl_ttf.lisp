;;;; SDL_ttf2.0 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-ttf)

(defcfun ("TTF_Init" TTF_Init) :int)

(defcfun ("TTF_Quit" TTF_Quit) :void)

(defcfun ("TTF_OpenFont" TTF_OpenFont) :pointer
  (filename :string)
  (ptsize :int)) ; ptsize is basically pixel-height

(defcfun ("TTF_CloseFont" TTF_CloseFont) :pointer
  (font :pointer))

(defcfun ("TTF_RenderText_Solid" TTF_RenderText_Solid) :pointer
  (font :pointer)
  (text :string)
  (fg :pointer))

