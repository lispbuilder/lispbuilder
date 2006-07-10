;; SDL_mixer library using CFFI for foreign function interfacing...
;; (C)2006 Luke Crook <luke@balooga.com>
;; Thanks to Frank Buss and Surendra Singh
;; see COPYING for license
;; This file contains some useful functions for using SDL from Common lisp
;; using sdl_mixer.lisp (the CFFI wrapper)

(in-package #:lispbuilder-sdl-mixer) 

;;;; Overrides to C header files follow:
;;;;

;;;; "SDL_mixer.h"
;;;; Must define the CPU byte order.
(defun SDL_MIXER_VERSION (x)
  (cffi:with-foreign-slots ((sdl:major sdl:minor sdl:patch) x sdl:SDL_version)
    (setf sdl:major SDL_MIXER_MAJOR_VERSION
          sdl:minor SDL_MIXER_MINOR_VERSION
          sdl:patch SDL_MIXER_PATCHLEVEL)))

(defun MIX_VERSION (x)
  (SDL_MIXER_VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant MIX_DEFAULT_FORMAT sdl:AUDIO_S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant MIX_DEFAULT_FORMAT sdl:AUDIO_S16LSB) ;; Little Endian

(defun Mix_LoadWAV (file)
  (Mix_LoadWAV_RW (sdl:SDL_RWFROMFILE file "rb") 1))

(defconstant MIX_EFFECTSMAXSPEED "MIX_EFFECTSMAXSPEED")

(defun Mix_PlayChannel (channel chunk loops)
  (Mix_PlayChannelTimed channel chunk loops -1))

(defun Mix_FadeInChannel (channel chunk loops ms)
  (Mix_FadeInChannelTimed channel chunk loops ms -1))

(defun Mix_GetError ()
  (sdl:SDL_GetError))
;;;; end "SDL_mixer.h"

;;;;
;;;; end Overrides
