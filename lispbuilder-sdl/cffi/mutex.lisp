
(in-package #:lispbuilder-sdl-cffi)

(cl:defconstant SDL-MUTEX-TIMEDOUT 1)

;;#define SDL_MUTEX_MAXWAIT	(~(Uint32)0)

(cffi:defcstruct SDL-Mutex
  (id :unsigned-int))

(cffi:defcfun ("SDL_CreateMutex" SDL-Create-Mutex) (:pointer (:struct SDL-Mutex)))

(cffi:defcfun ("SDL_mutexP" SDL-mutexP) return->=0-as-t
  (mutex (:pointer (:struct SDL-Mutex))))

(cffi:defcfun ("SDL_mutexV" SDL-mutexV) return->=0-as-t
  (mutex (:pointer (:struct SDL-Mutex))))

(cffi:defcfun ("SDL_DestroyMutex" SDL-Destroy-Mutex) :void
  (mutex (:pointer (:struct SDL-Mutex))))

(cffi:defcstruct SDL-semaphore
  (id :unsigned-int))

(cffi:defcfun ("SDL_CreateSemaphore" SDL-Create-Semaphore) (:pointer (:struct SDL-semaphore))
  (initial-value :unsigned-int))

(cffi:defcfun ("SDL_DestroySemaphore" SDL-Destroy-Semaphore) :void
  (semaphore (:pointer (:struct SDL-semaphore))))

(cffi:defcfun ("SDL_SemWait" SDL-Sem-Wait) :int
  (semaphore (:pointer (:struct SDL-semaphore))))

(cffi:defcfun ("SDL_SemTryWait" SDL-Sem-Try-Wait) :int
  (semaphore (:pointer (:struct SDL-semaphore))))

(cffi:defcfun ("SDL_SemWaitTimeout" SDL-Sem-Wait-Timeout) :int
  (semaphore (:pointer (:struct SDL-semaphore)))
  (ms :unsigned-int))

(cffi:defcfun ("SDL_SemPost" SDL-Sem-Post) :int
  (semaphore (:pointer (:struct SDL-semaphore))))

(cffi:defcfun ("SDL_SemValue" SDL-Sem-Value) :unsigned-int
  (semaphore (:pointer (:struct SDL-semaphore))))
