;;;; SDL_ttf2.0 CFFI lisp wrapper

(in-package #:lispbuilder-sdl-ttf)

(defctype sdl-version :pointer)
(defctype ttf-font :pointer)
(defctype ttf-return-val-0-1 :int)
(defctype ttf-return-val-0+1 :int)
(defctype ttf-font-style :int)
(defctype sdl-color sdl::sdl_color)
(defctype ttf-swapped-unicode :int)

;; #define SDL_TTF_MAJOR_VERSION	2
(defconstant MAJOR-VERSION 2)

;; #define SDL_TTF_MINOR_VERSION	0
(defconstant MINOR-VERSION 0)

;; #define SDL_TTF_PATCHLEVEL	8
(defconstant PATCHLEVEL 0)

(defun VERSION (x)
  (setf (cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:major) MAJOR-VERSION
	(cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:minor) MINOR-VERSION
	(cffi:foreign-slot-value x 'sdl:sdl_version 'sdl:patch) PATCHLEVEL)
  x)

;; This function gets the version of the dynamically linked SDL_ttf library.
;; it should NOT be used to fill a version structure, instead you should
;; use the SDL_TTF_VERSION() macro.
;; extern DECLSPEC const SDL_version * SDLCALL TTF_Linked_Version(void);
(defcfun ("TTF_Linked_Version" Linked-Version) sdl-version)

;; #define UNICODE_BOM_NATIVE	0xFEFF
(defconstant UNICODE_BOM_NATIVE #xFEFF)

;; #define UNICODE_BOM_SWAPPED	0xFFFE
(defconstant UNICODE_BOM_SWAPPED #xFFFE)

;; This function tells the library whether UNICODE text is generally
;; byteswapped.  A UNICODE BOM character in a string will override
;; this setting for the remainder of that string.
;; extern DECLSPEC void SDLCALL TTF_ByteSwappedUNICODE(int swapped);
(defcfun ("TTF_ByteSwappedUNICODE" Byte-Swapped-UNICODE) :void
  (swapped ttf-swapped-unicode))

;; Initialize the TTF engine - returns t if successful, NIL on error
;; extern DECLSPEC int SDLCALL TTF_Init(void);
(defcfun ("TTF_Init" Init) ttf-return-val-0-1)

;; Open a font file and create a font of the specified point size.
;; Some .fon fonts will have several sizes embedded in the file, so the
;; point size becomes the index of choosing which size.  If the value
;; is too high, the last indexed size will be the default.
;; extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFont(const char *file, int ptsize);
(defcfun ("TTF_OpenFont" OpenFont) ttf-font
  (file :string)
  (ptsize :int))

;; extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndex(const char *file, int ptsize, long index);
(defcfun ("TTF_OpenFontIndex" Open-Font-Index) ttf-font
  (file :string)
  (ptsize :int)
  (index :long))

;; extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize);
(defcfun ("TTF_OpenFontRW" Open-Font-RW) ttf-font
  (src sdl::SDL-RWops)
  (freesrc :int)
  (ptsize :int))

;; extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndexRW(SDL_RWops *src, int freesrc, int ptsize, long index);
(defcfun ("TTF_OpenFontIndexRW" Open-Font-Index-RW) ttf-font
  (src sdl::SDL-RWops)
  (freesrc :int)
  (ptsize :int)
  (index :long))

;; Set and retrieve the font style
;; This font style is implemented by modifying the font glyphs, and
;; doesn't reflect any inherent properties of the truetype font file.
;; #define TTF_STYLE_NORMAL	0x00
(defconstant TTF_STYLE_NORMAL #x00)

;; #define TTF_STYLE_BOLD		0x01
(defconstant TTF_STYLE_BOLD #x01)

;; #define TTF_STYLE_ITALIC	0x02
(defconstant TTF_STYLE_ITALIC #x02)

;; #define TTF_STYLE_UNDERLINE	0x04
(defconstant TTF_STYLE_UNDERLINE #x04)

;; extern DECLSPEC int SDLCALL TTF_GetFontStyle(TTF_Font *font);
(defcfun ("TTF_GetFontStyle" GetFontStyle) ttf-font-style
  (font ttf-font))

;; extern DECLSPEC void SDLCALL TTF_SetFontStyle(TTF_Font *font, int style) ;
(defcfun ("TTF_SetFontStyle" SetFontStyle) :void
  (font ttf-font)
  (style ttf-font-style))

;; Get the total height of the font - usually equal to point size
;; extern DECLSPEC int SDLCALL TTF_FontHeight(TTF_Font *font);
(defcfun ("TTF_FontHeight" getFontHeight) :int
  (font ttf-font))

;; Get the offset from the baseline to the top of the font
;; This is a positive value, relative to the baseline.
;; extern DECLSPEC int SDLCALL TTF_FontAscent(TTF_Font *font);
(defcfun ("TTF_FontAscent" getFontAscent) :int
  (font ttf-font))

;; Get the offset from the baseline to the bottom of the font
;; This is a negative value, relative to the baseline.
;; extern DECLSPEC int SDLCALL TTF_FontDescent(TTF_Font *font);
(defcfun ("TTF_FontDescent" getFontDescent) :int
  (font ttf-font))

;; Get the recommended spacing between lines of text for this font
;; extern DECLSPEC int SDLCALL TTF_FontLineSkip(TTF_Font *font);
(defcfun ("TTF_FontLineSkip" getFontLineSkip) :int
  (font ttf-font))

;; Get the number of faces of the font
;; extern DECLSPEC long SDLCALL TTF_FontFaces(TTF_Font *font);
(defcfun ("TTF_FontFaces" getFontFaces) :long
  (font ttf-font))

;; Get the font face attributes, if any
;; extern DECLSPEC int SDLCALL TTF_FontFaceIsFixedWidth(TTF_Font *font);
(defcfun ("TTF_FontFaceIsFixedWidth" getFontFaceIsFixedWidth) ttf-return-val-0+1
  (font ttf-font))

;; extern DECLSPEC char * SDLCALL TTF_FontFaceFamilyName(TTF_Font *font);
(defcfun ("TTF_FontFaceFamilyName" getFontFaceFamilyName) :string
  (font ttf-font))

;; extern DECLSPEC char * SDLCALL TTF_FontFaceStyleName(TTF_Font *font) ;
(defcfun ("TTF_FontFaceStyleName" getFontFaceStyleName) :string
  (font ttf-font))

;; Get the metrics (dimensions) of a glyph
;; To understand what these metrics mean, here is a useful link:
;; http://freetype.sourceforge.net/freetype2/docs/tutorial/step2.html
;; extern DECLSPEC int SDLCALL TTF_GlyphMetrics(TTF_Font *font, Uint16 ch,
;; 				     int *minx, int *maxx,
;;                                      int *miny, int *maxy, int *advance);
(defcfun ("TTF_GlyphMetrics" GlyphMetrics) ttf-return-val-0-1
  (font ttf-font)
  (ch :unsigned-short)
  (minx :pointer)
  (maxx :pointer)
  (miny :pointer)
  (maxy :pointer)
  (advance :pointer))

;; Get the dimensions of a rendered string of text
;; extern DECLSPEC int SDLCALL TTF_SizeText(TTF_Font *font, const char *text, int *w, int *h);
(defcfun ("TTF_SizeText" SizeText) ttf-return-val-0-1
  (font ttf-font)
  (text :string)
  (w :pointer)
  (h :pointer))

;; extern DECLSPEC int SDLCALL TTF_SizeUTF8(TTF_Font *font, const char *text, int *w, int *h);
(defcfun ("TTF_SizeUTF8" SizeUTF8) ttf-return-val-0-1
  (font ttf-font)
  (text :string)
  (w :pointer)
  (h :pointer))

;; extern DECLSPEC int SDLCALL TTF_SizeUNICODE(TTF_Font *font, const Uint16 *text, int *w, int *h)	;
(defcfun ("TTF_SizeUNICODE" SizeUNICODE) ttf-return-val-0-1
  (font ttf-font)
  (text :pointer)
  (w :pointer)
  (h :pointer))

;; Create an 8-bit palettized surface and render the given text at
;; fast quality with the given font and color.  The 0 pixel is the
;; colorkey, giving a transparent background, and the 1 pixel is set
;; to the text color.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Solid(TTF_Font *font,
;; 				const char *text, SDL_Color fg);
(defcfun ("TTF_RenderText_Solid" Render-Text-Solid) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Solid(TTF_Font *font,
;; 				const char *text, SDL_Color fg);
(defcfun ("TTF_RenderUTF8_Solid" Render-UTF8-Solid) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUNICODE_Solid(TTF_Font *font,
;; 				const Uint16 *text, SDL_Color fg);
(defcfun ("TTF_RenderUNICODE_Solid" RenderUNICODE-Solid) sdl::sdl-surface
  (font ttf-font)
  (text :pointer)
  (fg sdl-color))

;; Create an 8-bit palettized surface and render the given glyph at
;; fast quality with the given font and color.  The 0 pixel is the
;; colorkey, giving a transparent background, and the 1 pixel is set
;; to the text color.  The glyph is rendered without any padding or
;; centering in the X direction, and aligned normally in the Y direction.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderGlyph_Solid(TTF_Font *font,
;; 					Uint16 ch, SDL_Color fg);
(defcfun ("TTF_RenderGlyph_Solid" Render-Glyph-Solid) sdl::sdl-surface
  (font ttf-font)
  (ch :unsigned-short)
  (fg sdl-color))

;; Create an 8-bit palettized surface and render the given text at
;; high quality with the given font and colors.  The 0 pixel is background,
;; while other pixels have varying degrees of the foreground color.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Shaded(TTF_Font *font,
;; 				const char *text, SDL_Color fg, SDL_Color bg);
(defcfun ("TTF_RenderText_Shaded" Render-Text-Shaded) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color)
  (bg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Shaded(TTF_Font *font,
;; 								     const char *text, SDL_Color fg, SDL_Color bg) ;
(defcfun ("TTF_RenderUTF8_Shaded" Render-UTF8-Shaded) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color)
  (bg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUNICODE_Shaded(TTF_Font *font,
;; 									const Uint16 *text, SDL_Color fg, SDL_Color bg)	;
(defcfun ("TTF_RenderUNICODE_Shaded" RenderUNICODE-Shaded) sdl::sdl-surface
  (font ttf-font)
  (text :pointer)
  (fg sdl-color)
  (bg sdl-color))

;; Create an 8-bit palettized surface and render the given glyph at
;; high quality with the given font and colors.  The 0 pixel is background,
;; while other pixels have varying degrees of the foreground color.
;; The glyph is rendered without any padding or centering in the X
;; direction, and aligned normally in the Y direction.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderGlyph_Shaded(TTF_Font *font,
;; 				Uint16 ch, SDL_Color fg, SDL_Color bg);
(defcfun ("TTF_RenderGlyph_Shaded" Render-Glyph-Shaded) sdl::sdl-surface
  (font ttf-font)
  (ch :unsigned-short)
  (fg sdl-color)
  (bg sdl-color))

;; Create a 32-bit ARGB surface and render the given text at high quality,
;; using alpha blending to dither the font with the given color.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderText_Blended(TTF_Font *font,
;; 				const char *text, SDL_Color fg);
(defcfun ("TTF_RenderText_Blended" Render-Text-Blended) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUTF8_Blended(TTF_Font *font,
;; 								      const char *text, SDL_Color fg) ;
(defcfun ("TTF_RenderUTF8_Blended" Render-UTF8-Blended) sdl::sdl-surface
  (font ttf-font)
  (text :string)
  (fg sdl-color))

;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderUNICODE_Blended(TTF_Font *font,
;; 				const Uint16 *text, SDL_Color fg);
(defcfun ("TTF_RenderUNICODE_Blended" RenderUNICODE-Blended) sdl::sdl-surface
  (font ttf-font)
  (text :pointer)
  (fg sdl-color))

;; Create a 32-bit ARGB surface and render the given glyph at high quality,
;; using alpha blending to dither the font with the given color.
;; The glyph is rendered without any padding or centering in the X
;; direction, and aligned normally in the Y direction.
;; This function returns the new surface, or NULL if there was an error.
;; extern DECLSPEC SDL_Surface * SDLCALL TTF_RenderGlyph_Blended(TTF_Font *font,
;; 						Uint16 ch, SDL_Color fg);
(defcfun ("TTF_RenderGlyph_Blended" Render-Glyph-Blended) sdl::sdl-surface
  (font ttf-font)
  (ch :unsigned-short)
  (fg sdl-color))

;; Close an opened font file
;; extern DECLSPEC void SDLCALL TTF_CloseFont(TTF_Font *font);
(defcfun ("TTF_CloseFont" Close-Font) :void
  (font ttf-font))

;; De-initialize the TTF engine
;; extern DECLSPEC void SDLCALL TTF_Quit(void);
(defcfun ("TTF_Quit" Quit) :void)

;; Check if the TTF engine is initialized
;; extern DECLSPEC int SDLCALL TTF_WasInit(void);
(defcfun ("TTF_WasInit" is-Init) ttf-return-val-0+1)

;; /* We'll use SDL for reporting errors */
;; #define TTF_SetError	SDL_SetError
;; #define TTF_GetError	SDL_GetError
