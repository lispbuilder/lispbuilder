
(in-package #:lispbuilder-sdl-cffi)

(cl:defconstant SDL-MUTEX-TIMEDOUT 1)

;;#define SDL_MUTEX_MAXWAIT	(~(Uint32)0)

(cffi:defcstruct SDL-Mutex
  (id :unsigned-int))

(cffi:defcfun ("SDL_CreateMutex" SDL-Create-Mutex) SDL-Mutex)

(cffi:defcfun ("SDL_mutexP" SDL-mutexP) return->=0-as-t
  (mutex SDL-Mutex))

(cffi:defcfun ("SDL_mutexV" SDL-mutexV) return->=0-as-t
  (mutex SDL-Mutex))

(cffi:defcfun ("SDL_DestroyMutex" SDL-Destroy-Mutex) :void
  (mutex SDL-Mutex))

(cffi:defcstruct SDL-semaphore
  (id :unsigned-int))

(cffi:defcfun ("SDL_CreateSemaphore" SDL-Create-Semaphore) SDL-semaphore
  (initial-value :unsigned-int))

(cffi:defcfun ("SDL_DestroySemaphore" SDL-Destroy-Semaphore) :void
  (semaphore SDL-semaphore))

(cffi:defcfun ("SDL_SemWait" SDL-Sem-Wait) :int
  (semaphore SDL-semaphore))

(cffi:defcfun ("SDL_SemTryWait" SDL-Sem-Try-Wait) :int
  (semaphore SDL-semaphore))

(cffi:defcfun ("SDL_SemWaitTimeout" SDL-Sem-Wait-Timeout) :int
  (semaphore SDL-semaphore)
  (ms :unsigned-int))

(cffi:defcfun ("SDL_SemPost" SDL-Sem-Post) :int
  (semaphore SDL-semaphore))

(cffi:defcfun ("SDL_SemValue" SDL-Sem-Value) :unsigned-int
  (semaphore SDL-semaphore))
