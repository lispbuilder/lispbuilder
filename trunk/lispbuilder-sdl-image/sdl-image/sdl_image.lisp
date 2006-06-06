;;;; SDL_ttf2.0 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-image)

(defctype sdl-version :pointer)
(defctype image-return-val-0-1 :int)
(defctype image-return-val-0+1 :int)
(defctype image-type :string)
(defctype free-src :int)

;; #define SDL_IMAGE_MAJOR_VERSION	1
(defconstant MAJOR-VERSION 1)

;; #define SDL_IMAGE_MINOR_VERSION	2
(defconstant MINOR-VERSION 2)

;; #define SDL_IMAGE_PATCHLEVEL	5
(defconstant PATCHLEVEL 5)

(defun VERSION (x)
  (setf (cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:major) MAJOR-VERSION
	(cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:minor) MINOR-VERSION
	(cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:patch) PATCHLEVEL)
  x)

;; This function gets the version of the dynamically linked SDL_image library.
;; it should NOT be used to fill a version structure, instead you should
;; use the SDL_IMAGE_VERSION() macro.
;; extern DECLSPEC const SDL_version * SDLCALL IMG_Linked_Version(void);
(defcfun ("IMG_Linked_Version" Linked-Version) sdl-version)

;; Load an image from an SDL data source.
;; The 'type' may be one of: "BMP", "GIF", "PNG", etc.
;; If the image format supports a transparent pixel, SDL will set the
;; colorkey for the surface.  You can enable RLE acceleration on the
;; surface afterwards by calling:
;;   SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type);
(defcfun ("IMG_LoadTyped_RW" Load-Typed-RW) sdl::sdl-surface
  (src sdl::SDL-RWops)
  (freesrc free-src)
  (type image-type))

;; Convenience functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load(const char *file);
(defcfun ("IMG_Load" Load-img) sdl::sdl-surface
  (file :string))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load_RW(SDL_RWops *src, int freesrc);
(defcfun ("IMG_Load_RW" Load-RW) sdl::sdl-surface
  (src sdl::SDL-RWops)
  (freesrc free-src))

;; Invert the alpha of a surface for use with OpenGL
;; This function is now a no-op, and only provided for backwards compatibility.
;; extern DECLSPEC int SDLCALL IMG_InvertAlpha(int on);
;; (defcfun ("IMG_InvertAlpha" IMG_InvertAlpha) :int
;;   (on :int))

;; Functions to detect a file type, given a seekable source
;; extern DECLSPEC int SDLCALL IMG_isBMP(SDL_RWops *src);
(defcfun ("IMG_isBMP" isBMP) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isGIF(SDL_RWops *src) ;
(defcfun ("IMG_isGIF" isGIF) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isJPG(SDL_RWops *src)
(defcfun ("IMG_isJPG" isJPG) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isLBM(SDL_RWops *src) ;
(defcfun ("IMG_isLBM" isLBM) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPCX(SDL_RWops *src) ;
(defcfun ("IMG_isPCX" isPCX) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPNG(SDL_RWops *src) ;
(defcfun ("IMG_isPNG" isPNG) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPNM(SDL_RWops *src) ;
(defcfun ("IMG_isPNM" isPNM) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isTIF(SDL_RWops *src) ;
(defcfun ("IMG_isTIF" isTIF) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXCF(SDL_RWops *src) ;
(defcfun ("IMG_isXCF" isXCF) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXPM(SDL_RWops *src) ;
(defcfun ("IMG_isXPM" isXPM) image-return-val-0+1
  (src sdl::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXV(SDL_RWops *src) ;
(defcfun ("IMG_isXV" isXV) image-return-val-0+1
  (src sdl::SDL-RWops))

;; Individual loading functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadBMP_RW(SDL_RWops *src);
(defcfun ("IMG_LoadBMP_RW" Load-BMP-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadGIF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadGIF_RW" Load-GIF-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadJPG_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadJPG_RW" Load-JPG-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadLBM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadLBM_RW" Load-LBM-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPCX_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPCX_RW" Load-PCX-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNG_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPNG_RW" Load-PNG-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPNM_RW" Load-PNM-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTGA_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadTGA_RW" Load-TGA-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTIF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadTIF_RW" Load-TIF-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXCF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadXCF_RW" Load-XCF-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXPM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadXPM_RW" Load-XPM-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXV_RW(SDL_RWops *src) ;-
(defcfun ("IMG_LoadXV_RW" Load-XV-RW) sdl::sdl-surface
  (src sdl::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_ReadXPMFromArray(char **xpm);
(defcfun ("IMG_ReadXPMFromArray" Read-XPM-FromArray) sdl::sdl-surface
  (char :pointer))

;; We'll use SDL for reporting errors
;; #define IMG_SetError	SDL_SetError
;; #define IMG_GetError	SDL_GetError

