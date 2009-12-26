// lispbuilder-sdl-glue.cpp : Defines the exported functions for the DLL application.
//

/* Copyright (c) 2009, Luke J Crook.  All rights reserved. 

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


#include "lispbuilder-sdl-glue.h"

/* Set up for C function definitions, even when using C++ */
#ifdef __cplusplus
extern "C" {
#endif

LISPBUILDERSDLGLUE_API int SDL_glue_SDL_WaitUntilBufferFull(void) {
    /*	Let lispbulder-sdl know to fill the audio buffer */
	SDL_mutexP(buffer_fill_lock);
	buffer_fill = 1;
	SDL_mutexV(buffer_fill_lock);
	
	/*	And wait until lispbuilder-sdl has done so */
	return SDL_SemWait(audio_buffer_lock);
}
LISPBUILDERSDLGLUE_API int SDL_glue_SDL_BufferFilled(void) {
	/* lispbuilder-sdl calls this function *after* filling the audio buffer. */
	SDL_mutexP(buffer_fill_lock);
	buffer_fill = -1;
	SDL_mutexV(buffer_fill_lock);
	
	/* Wake-up the callback */
	return SDL_SemPost(audio_buffer_lock);
}

LISPBUILDERSDLGLUE_API int SDL_glue_SDL_RequireBufferFill(void) {
	return buffer_fill;
}

LISPBUILDERSDLGLUE_API Uint8* SDL_glue_SDL_GetAudioBuffer(void) {
	return audio_buffer;
}

LISPBUILDERSDLGLUE_API int SDL_glue_SDL_GetAudioBufferLength(void) {
	return audio_buffer_length;
}

LISPBUILDERSDLGLUE_API void SDL_glue_mixaudio(void *unused, Uint8 *stream, int len) {
	/*	I can probably get rid of this by allocating the global audio buffer using the buffer
		size in the allocated audio mixer. An optimization for later. */
	if (audio_buffer == NULL) {
		audio_buffer = (Uint8 *)malloc(len);
		audio_buffer_length = len;
	}
	/*	Thought we may need to initialize the global audio buffer with silence each call.
		Turns out that this is not needed when the lispbuilder-sdl game loop is fast enought fill the audio buffer. 
		If the game loop is too slow, then the audio will break up regardless. */
//	for (int i=0;i<len;i++) {
//        audio_buffer[i] = stream[i];
//    }

	/*	Wait until given the signal that lispbuilder-sdl has filled the audio buffer */
	SDL_glue_SDL_WaitUntilBufferFull();

	/* Copy the audio buffer into the callback. */
	for (int i=0;i<len;i++) {
        stream[i] = audio_buffer[i];
    }
}

LISPBUILDERSDLGLUE_API int SDLCALL SDL_glue_SDL_OpenAudio(SDL_AudioSpec *desired, SDL_AudioSpec *obtained) {
	buffer_fill_lock = SDL_CreateMutex();
	buffer_fill = -1;
	audio_buffer_lock = SDL_CreateSemaphore(1);
	desired->callback = SDL_glue_mixaudio;
	return SDL_OpenAudio(desired, obtained);
}

LISPBUILDERSDLGLUE_API void SDLCALL SDL_glue_SDL_CloseAudio(void) {
	/* Seems this sequence of calls is quite important, or the glue-library will hang.
		1) Bump the semaphore in case the callback is waiting on audio data.
		2) Lock the callback. In effect returns when the callback returns. 
		   Also makes sure that the callback cannot be called again before we hav a chance to
		   close the audio device. */
	SDL_SemPost(audio_buffer_lock);
	SDL_LockAudio();

	if (audio_buffer_lock != NULL) {
        SDL_DestroySemaphore(audio_buffer_lock);
        audio_buffer_lock = NULL;
	}
	if (buffer_fill_lock != NULL) {
        SDL_DestroyMutex(buffer_fill_lock);
	    buffer_fill_lock = NULL;
	}
	/*	It is very likely that the audio buffer will be a different size the next time the audio device is opened
		(assuming different input parameters. */
	if (audio_buffer != NULL) { 
		free(audio_buffer);
		audio_buffer = NULL;
	}
	SDL_CloseAudio();
}

/* Ends C function definitions when using C++ */
#ifdef __cplusplus
}
#endif

