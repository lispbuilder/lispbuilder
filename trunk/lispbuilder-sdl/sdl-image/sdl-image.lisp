;;;; SDL_image v1.2.10 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-cffi)

(defvar *image-format-support* (list :GIF :JPG :LBM :PCX :PNG :PNM :TGA :TIF :XCF :XPM :XV))

(defun convert-image-type (value)
  (values (case value
	    (:ICO (cffi:foreign-string-alloc "ICO"))
	    (:CUR (cffi:foreign-string-alloc "CUR"))
            (:TGA (cffi:foreign-string-alloc "TGA"))
	    (:BMP (cffi:foreign-string-alloc "BMP"))
	    (:PNM (cffi:foreign-string-alloc "PNM"))
	    (:XPM (cffi:foreign-string-alloc "XPM"))
	    (:XCF (cffi:foreign-string-alloc "XCF"))
	    (:PCX (cffi:foreign-string-alloc "PCX"))
	    (:GIF (cffi:foreign-string-alloc "GIF"))
	    (:JPG (cffi:foreign-string-alloc "JPG"))
	    (:TIF (cffi:foreign-string-alloc "TIF"))
	    (:LBM (cffi:foreign-string-alloc "LBM"))
	    (:PNG (cffi:foreign-string-alloc "PNG"))
	    (otherwise (cffi:foreign-string-alloc "")))
	  t))

(defmethod cffi:free-translated-object (ptr (name (eql 'image-type)) free-p)
  (if free-p
      (cffi:foreign-string-free ptr)))

(defun return-val-0-1 (value)
  (when (= value 0)
    t))

(defun return-val-0+1 (value)
  (unless (= value 0)
    t))

(defctype image-return-val-0+1 (:wrapper :int :from-c return-val-0+1))
(defctype image-type (:wrapper :string :to-c convert-image-type))
(defctype free-src :boolean)

(defconstant SDL-IMAGE-MAJOR-VERSION 1)
(defconstant SDL-IMAGE-MINOR-VERSION 2)
(defconstant SDL-IMAGE-PATCH-LEVEL 10)

(defun set-image-version (x)
  (with-foreign-slots ((major minor patch) x sdl-version)
    (setf major sdl-image-major-version
	  minor sdl-image-minor-version
	  patch sdl-image-patch-level))
  x)


;; This function gets the version of the dynamically linked SDL_image library.
;; it should NOT be used to fill a version structure, instead you should
;; use the SDL_IMAGE_VERSION() macro.
;; extern DECLSPEC const SDL_version * SDLCALL IMG_Linked_Version(void);
(defun image-linked-version ()
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Linked_Version" :library 'sdl-image)
                                  () sdl-version)))

(defun image-library-version ()
  (when *image-loaded-p*
    (get-library-version (image-linked-version))))

(defun image-wrapper-version ()
  (version-number sdl-image-major-version
                  sdl-image-minor-version
                  sdl-image-patch-level))

(defun image-version-at-least (x y z)
  (when (>= (image-library-version) (version-number x y z))
      t))

(cffi:defbitfield image-init-flags
  (:jpg #x01)
  (:png #x02)
  (:tif #x04))

;; Load an image from an SDL data source.
;; The 'type' may be one of: "BMP", "GIF", "PNG", etc.
;; If the image format supports a transparent pixel, SDL will set the
;; colorkey for the surface.  You can enable RLE acceleration on the
;; surface afterwards by calling:
;;   SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type);
(defun image-load-typed-rw (src freesrc type)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadTyped_RW" :library 'sdl-image)
				  () SDL-RWops src
                                  free-src freesrc
                                  image-type type
                                  sdl-surface)))

;; Convenience functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load(const char *file);
(defun image-load-image (file)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Load" :library 'sdl-image)
				  () :string file
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_Load_RW(SDL_RWops *src, int freesrc);
(defun image-load-rw (src freesrc)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_Load_RW" :library 'sdl-image)
				  () SDL-RWops src
                                  free-src freesrc
                                  sdl-surface)))

;; extern DECLSPEC int SDLCALL IMG_isICO(SDL_RWops *src);
(defun image-is-ico (source)
  (when (and *image-loaded-p* (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-cffi::sdl-image))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isICO" :library 'sdl-image)
					     () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isCUR(SDL_RWops *src);
(defun image-is-cur (source)
  (when (and *image-loaded-p* (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-cffi::sdl-image))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isCUR" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; Functions to detect a file type, given a seekable source
;; extern DECLSPEC int SDLCALL IMG_isBMP(SDL_RWops *src);
(defun image-is-bmp (source)
  (when *image-loaded-p*
    (when (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isBMP" :library 'sdl-image)
					   () SDL-RWops source :boolean)
      t)))

;; extern DECLSPEC int SDLCALL IMG_isGIF(SDL_RWops *src) ;
(defun image-is-gif (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isGIF" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isJPG(SDL_RWops *src)
(defun image-is-jpg (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isJPG" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isLBM(SDL_RWops *src) ;
(defun image-is-lbm (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isLBM" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isPCX(SDL_RWops *src) ;
(defun image-is-pcx (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isPCX" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isPNG(SDL_RWops *src) ;
(defun image-is-png (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isPNG" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isPNM(SDL_RWops *src) ;
(defun image-is-pnm (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isPNM" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isTIF(SDL_RWops *src) ;
(defun image-is-tif (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isTIF" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isXCF(SDL_RWops *src) ;
(defun image-is-xcf (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isXCF" :library 'sdl-image)
                                  () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isXPM(SDL_RWops *src) ;
(defun image-is-xpm (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isXPM" :library 'sdl-image)
					   () SDL-RWops source :boolean)))

;; extern DECLSPEC int SDLCALL IMG_isXV(SDL_RWops *src) ;
(defun image-is-xv (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_isXV" :library 'sdl-image)
                                  () SDL-RWops source :boolean)))

;; Individual loading functions
;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadICO_RW(SDL_RWops *src);
(defun image-load-ico-rw (source)
  (when (and *image-loaded-p* (cffi:foreign-symbol-pointer "IMG_LoadICO_RW" :library 'sdl-image))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadICO_RW" :library 'sdl-image)
					   () SDL-RWops source
                                           sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadCUR_RW(SDL_RWops *src);
(defun image-load-cur-rw (source)
  (when (and *image-loaded-p* (cffi:foreign-symbol-pointer "IMG_LoadCUR_RW" :library 'sdl-image))
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadCUR_RW" :library 'sdl-image)
					   () SDL-RWops source
                                           sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadBMP_RW(SDL_RWops *src);
(defun image-load-bmp-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadBMP_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadGIF_RW(SDL_RWops *src);
(defun image-load-gif-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadGIF_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadJPG_RW(SDL_RWops *src) ;
(defun image-load-jpg-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadJPG_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadLBM_RW(SDL_RWops *src) ;
(defun image-load-lbm-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadLBM_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPCX_RW(SDL_RWops *src) ;
(defun image-load-pcx-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadPCX_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNG_RW(SDL_RWops *src) ;
(defun image-load-png-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadPNG_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadPNM_RW(SDL_RWops *src) ;
(defun image-load-pnm-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadPNM_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTGA_RW(SDL_RWops *src) ;
(defun image-load-tga-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadTGA_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadTIF_RW(SDL_RWops *src) ;
(defun image-load-tif-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadTIF_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXCF_RW(SDL_RWops *src) ;
(defun image-load-xcf-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadXCF_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXPM_RW(SDL_RWops *src) ;
(defun image-load-xpm-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadXPM_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_LoadXV_RW(SDL_RWops *src) ;-
(defun image-load-xv-rw (source)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_LoadXV_RW" :library 'sdl-image)
                                  () SDL-RWops source
                                  sdl-surface)))

;; extern DECLSPEC SDL_Surface * SDLCALL IMG_ReadXPMFromArray(char **xpm);
(defun image-read-xpm-from-array (char)
  (when *image-loaded-p*
    (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "IMG_ReadXPMFromArray" :library 'sdl-image)
                                  () :pointer char
                                  sdl-surface)))
