;;;; SDL_image v1.2.6 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-image-cffi)

(defctype sdl-version :pointer)
(defctype image-return-val-0+1 (:wrapper :int :from-c return-val-0+1))
(defctype image-type (:wrapper :string :to-c convert-image-type))
(defctype free-src :boolean)

(defconstant SDL-IMAGE-MAJOR-VERSION 1)
(defconstant SDL-IMAGE-MINOR-VERSION 2)
(defconstant SDL-IMAGE-PATCH-LEVEL 6)

(defun VERSION (x)
  (with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) x sdl-cffi::SDL-version)
    (setf sdl-cffi::major SDL-IMAGE-MAJOR-VERSION
	  sdl-cffi::minor SDL-IMAGE-MINOR-VERSION
	  sdl-cffi::patch SDL-IMAGE-PATCH-LEVEL))
  x)

;; This function gets the version of the dynamically linked SDL_image library.
;; it should NOT be used to fill a version structure, instead you should
;; use the SDL_IMAGE_VERSION() macro.
;; extern DECLSPEC const SDL_version * SDLCALL IMG_Linked_Version(void);
(defcfun ("IMG_Linked_Version" IMG-Linked-Version) sdl-version)

;; Load an image from an SDL data source.
;; The 'type' may be one of: "BMP", "GIF", "PNG", etc.
;; If the image format supports a transparent pixel, SDL will set the
;; colorkey for the surface.  You can enable RLE acceleration on the
;; surface afterwards by calling:
;;   SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type);
(defcfun ("IMG_LoadTyped_RW" IMG-Load-Typed-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops)
  (freesrc free-src)
  (type image-type))

;; Convenience functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load(const char *file);
(defcfun ("IMG_Load" IMG-Load-img) sdl-cffi::sdl-surface
  (file :string))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load_RW(SDL_RWops *src, int freesrc);
(defcfun ("IMG_Load_RW" IMG-Load-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops)
  (freesrc free-src))

;; Functions to detect a file type, given a seekable source
;; extern DECLSPEC int SDLCALL IMG_isBMP(SDL_RWops *src);
(defcfun ("IMG_isBMP" IMG-is-BMP) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isGIF(SDL_RWops *src) ;
(defcfun ("IMG_isGIF" IMG-is-GIF) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isJPG(SDL_RWops *src)
(defcfun ("IMG_isJPG" IMG-is-JPG) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isLBM(SDL_RWops *src) ;
(defcfun ("IMG_isLBM" IMG-is-LBM) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPCX(SDL_RWops *src) ;
(defcfun ("IMG_isPCX" IMG-is-PCX) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPNG(SDL_RWops *src) ;
(defcfun ("IMG_isPNG" IMG-is-PNG) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isPNM(SDL_RWops *src) ;
(defcfun ("IMG_isPNM" IMG-is-PNM) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isTIF(SDL_RWops *src) ;
(defcfun ("IMG_isTIF" IMG-is-TIF) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXCF(SDL_RWops *src) ;
(defcfun ("IMG_isXCF" IMG-is-XCF) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXPM(SDL_RWops *src) ;
(defcfun ("IMG_isXPM" IMG-is-XPM) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC int SDLCALL IMG_isXV(SDL_RWops *src) ;
(defcfun ("IMG_isXV" IMG-is-XV) image-return-val-0+1
  (src sdl-cffi::SDL-RWops))

;; Individual loading functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadBMP_RW(SDL_RWops *src);
(defcfun ("IMG_LoadBMP_RW" IMG-Load-BMP-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadGIF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadGIF_RW" IMG-Load-GIF-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadJPG_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadJPG_RW" IMG-Load-JPG-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadLBM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadLBM_RW" IMG-Load-LBM-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPCX_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPCX_RW" IMG-Load-PCX-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNG_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPNG_RW" IMG-Load-PNG-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadPNM_RW" IMG-Load-PNM-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTGA_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadTGA_RW" IMG-Load-TGA-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTIF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadTIF_RW" IMG-Load-TIF-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXCF_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadXCF_RW" IMG-Load-XCF-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXPM_RW(SDL_RWops *src) ;
(defcfun ("IMG_LoadXPM_RW" IMG-Load-XPM-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXV_RW(SDL_RWops *src) ;-
(defcfun ("IMG_LoadXV_RW" IMG-Load-XV-RW) sdl-cffi::sdl-surface
  (src sdl-cffi::SDL-RWops))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_ReadXPMFromArray(char **xpm);
(defcfun ("IMG_ReadXPMFromArray" IMG-Read-XPM-From-Array) sdl-cffi::sdl-surface
  (char :pointer))

;; We'll use SDL for reporting errors
;; #define IMG_SetError	SDL_SetError
;; #define IMG_GetError	SDL_GetError