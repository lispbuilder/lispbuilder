
(in-package #:lispbuilder-sdl-cffi)

;;; These are really the only functions we require from "SDL_timer.h"
;;/* Get the number of milliseconds since the SDL library initialization.
;; * Note that this value wraps if the program runs for more than ~49 days.
;; */ 
;;extern DECLSPEC Uint32 SDLCALL SDL_GetTicks(void);
(defcfun ("SDL_GetTicks" SDL-Get-Ticks) :int)

;;/* Wait a specified number of milliseconds before returning */
;;extern DECLSPEC void SDLCALL SDL_Delay(Uint32 ms);
(defcfun ("SDL_Delay" SDL-Delay) :void
  (ms :int))


