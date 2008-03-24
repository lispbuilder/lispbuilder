
(in-package #:lispbuilder-sdl-cffi)

(cffi:defcstruct SDL-RWops
	(seek :pointer)
	(read :pointer)
	(write :pointer)
	(close :pointer)
	(type :unsigned-int)
	(hidden :pointer))

(cffi:defcunion SDL_RWops_hidden
	#+win32(win32io :pointer)
	#-win32 (have-stdio-h :pointer)
	(mem :pointer)
	(unknown :pointer))

(cffi:defcstruct SDL_RWops_win32io
	(append :int)
	(h :pointer)
	(buffer :pointer))

(cffi:defcstruct SDL_RWops_buffer
	(data :pointer)
	(size :int)
	(left :int))

(cffi:defcstruct SDL_RWops_have_stdio_h
	(autoclose :int)
	(file :pointer))

(cffi:defcstruct SDL_RWops_hidden_unknown
	(data1 :pointer))

(cffi:defcstruct SDL_RWops_hidden_mem
	(base :pointer)
	(here :pointer)
	(stop :pointer))

(cffi:defcfun ("SDL_RWFromFile" sdl-RW-From-File) :pointer
  (file sdl-string)
  (mode sdl-string))

(cffi:defcfun ("SDL_RWFromMem" SDL-RW-From-Mem) :pointer
  (mem :pointer)
  (size :int))

(cffi:defcfun ("SDL_RWFromConstMem" SDL-RW-From-Const-Mem) :pointer
  (mem :pointer)
  (size :int))

(cffi:defcfun ("SDL_AllocRW" SDL-Alloc-RW) :pointer)

(cffi:defcfun ("SDL_FreeRW" SDL-Free-RW) :void
  (area :pointer))

(cl:defconstant RW-SEEK-SET 0)

(cl:defconstant RW-SEEK-CUR 1)

(cl:defconstant RW-SEEK-END 2)

(cffi:defcfun ("SDL_ReadLE16" SDL_ReadLE16) :unsigned-short
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE16" SDL_ReadBE16) :unsigned-short
  (src :pointer))

(cffi:defcfun ("SDL_ReadLE32" SDL_ReadLE32) :unsigned-int
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE32" SDL_ReadBE32) :unsigned-int
  (src :pointer))

(cffi:defcfun ("SDL_ReadLE64" SDL_ReadLE64) :pointer
  (src :pointer))

(cffi:defcfun ("SDL_ReadBE64" SDL_ReadBE64) :pointer
  (src :pointer))

(cffi:defcfun ("SDL_WriteLE16" SDL_WriteLE16) :int
  (dst :pointer)
  (value :unsigned-short))

(cffi:defcfun ("SDL_WriteBE16" SDL_WriteBE16) :int
  (dst :pointer)
  (value :unsigned-short))

(cffi:defcfun ("SDL_WriteLE32" SDL_WriteLE32) :int
  (dst :pointer)
  (value :unsigned-int))

(cffi:defcfun ("SDL_WriteBE32" SDL_WriteBE32) :int
  (dst :pointer)
  (value :unsigned-int))

(cffi:defcfun ("SDL_WriteLE64" SDL_WriteLE64) :int
  (dst :pointer)
  (value :pointer))

(cffi:defcfun ("SDL_WriteBE64" SDL_WriteBE64) :int
  (dst :pointer)
  (value :pointer))
