/* Copyright (c) 2007, Luke J Crook.  All rights reserved. 

   Redistribution and use in source and binary forms, with or without 
   modification, are permitted provided that the following conditions 
   are met: 

     * Redistributions of source code must retain the above copyright 
       notice, this list of conditions and the following disclaimer. 

     * Redistributions in binary form must reproduce the above 
       copyright notice, this list of conditions and the following 
       disclaimer in the documentation and/or other materials 
       provided with the distribution. 

   THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED 
   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY 
   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */

#include "lispbuilder-sdl-ttf-glue.h"


extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderText_Solid(TTF_Font *font,
								const char *text, SDL_Color *fg) {
  return TTF_RenderText_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUTF8_Solid(TTF_Font *font,
								const char *text, SDL_Color *fg) {
  return TTF_RenderUTF8_Solid(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUNICODE_Solid(TTF_Font *font,
								   const Uint16 *text, SDL_Color *fg) {
  return TTF_RenderUNICODE_Solid(font, text, *fg);
}



extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderGlyph_Solid(TTF_Font *font,
								 Uint16 ch, SDL_Color *fg) {
  return TTF_RenderGlyph_Solid(font, ch, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderText_Shaded(TTF_Font *font,
								 const char *text, SDL_Color *fg, SDL_Color *bg) {
  return TTF_RenderText_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUTF8_Shaded(TTF_Font *font,
								 const char *text, SDL_Color *fg, SDL_Color *bg) {
  return TTF_RenderUTF8_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUNICODE_Shaded(TTF_Font *font,
								    const Uint16 *text, SDL_Color *fg, SDL_Color *bg) {
  return TTF_RenderUNICODE_Shaded(font, text, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderGlyph_Shaded(TTF_Font *font,
								  Uint16 ch, SDL_Color *fg, SDL_Color *bg) {
  return TTF_RenderGlyph_Shaded(font, ch, *fg, *bg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderText_Blended(TTF_Font *font,
								  const char *text, SDL_Color *fg) {
  return TTF_RenderText_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUTF8_Blended(TTF_Font *font,
								  const char *text, SDL_Color *fg) {
  return TTF_RenderUTF8_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderUNICODE_Blended(TTF_Font *font,
								     const Uint16 *text, SDL_Color *fg) {
  return TTF_RenderUNICODE_Blended(font, text, *fg);
}

extern DECLSPEC SDL_Surface * SDLCALL TTF_glue_RenderGlyph_Blended(TTF_Font *font,
								   Uint16 ch, SDL_Color *fg) {
  return TTF_RenderGlyph_Blended(font, ch, *fg);
}

