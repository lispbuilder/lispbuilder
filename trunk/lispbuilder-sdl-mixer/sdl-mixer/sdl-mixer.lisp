
;;;; SDL_mixer CFFI lisp wrapper
;;;; Part of the CL-Gardeners project
;;;; http://wiki.alu.org/Application_Builder
;;;; (C)2008 Luke J Crook <luke@balooga.com>
;;;; See COPYING for license
;;;;
;;;; This .i file successfully generates CFFI bindings for SDL_mixer version 1.2.8
;;;;
;;;; SWIG command line:
;;;; swig -cffi -I<path_to_SDL_mixer_includes> -I<path_to_SDL_includes> -Ilib -Ilib\cffi <location_of_openrmswig.i>

(in-package #:lispbuilder-sdl-mixer-cffi)

;;; This is to handle a C macro where 1 is shifted left n times
(defun 1<<(x) (ash 1 x))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'openrm-lispify)
(defun openrm-lispify (name flag &optional (package *package*))
  (labels ((find-sub (src lst)
	     (when (>= (length lst)
		       (length src))
	       (if (and (equal src (subseq lst 0 (length src)))
			(not (equal (nth (length src)
					 lst) #\_))
		        (not (null (nth (length src)
			     	        lst))))
		   t
		   nil)))
	   (replace-sub (new old lis)
	     (append new (nthcdr (length old) lis)))
	   (next-char (char)
	     (if char
		 (cond
		   ((upper-case-p char)
		    'upper)
		   ((lower-case-p char)
		    'lower)
		   (t nil))
		 nil))
	      (helper (lst last prev-last rest &aux (c (car lst)))
	        (declare (ignore prev-last))
		(cond
		  ((null lst)
		   rest)
		  ((upper-case-p c)
		   (let ((new '(#\R #\M #\_)) (old '(#\R #\M)))
		     (when (and (not (find-sub '(#\R #\M #\I #\_) lst))
				(find-sub old lst))
		       (setf lst (replace-sub new old lst)
			     c (first new))))		   
		   (helper (cdr lst) 'upper last
			   (cond
			     ((or (equal last 'lower)
;; 				  (equal last 'digit)
				  )
			      (list* c #\- rest))
			     ((and (equal last 'upper)
				   (equal (next-char (cadr lst)) 'lower))
			      (list* c #\- rest))
			     (t (cons c rest)))))
		  ((lower-case-p c)
		   (helper (cdr lst) 'lower last (cons (char-upcase c) rest)))
		  ((digit-char-p c)
		   (helper (cdr lst) 'digit last
			   (case last
			     ((upper lower) (list* c #\- rest))
			     (t (cons c rest)))))
		  ((char-equal c #\_)
		   (helper (cdr lst) '_ last (cons #\- rest)))
		  (t
		   (error "Invalid character: ~A" c)))))
    (let ((fix (case flag
		    ((constant variable) "+")
		    (enumvalue "")
		    (t ""))))
      (intern
       (concatenate
	'string
	fix
	(nreverse (helper (concatenate 'list name) nil nil nil))
	fix)
       package))))
))

;;;; Lispifies the following 'C' keywords:
;;;; scancode 		=    SCANCODE
;;;; SDL_ALL_HOTKEYS 	=    *SDL-ALL-HOTKEYS*
;;;; SDLKey 		=    SDL-KEY
;;;; RMenum		=    RM-ENUM
;;;; SDL_GetKeyRepeat	=    SDL-GET-KEY-REPEAT
;;;; SDL_RWFromFP	=    SDL-RW-FROM-FP
;;;; SDL_HasSSE 	=    SDL-HAS-SSE
;;;; SDL_HasSSE2 	=    SDL-HAS-SSE-2
;;;; RMcolor4D		=    RM-COLOR-4D
;;;; SDL_Has3DNow 	=    SDL-HAS-3D-NOW
;;;; SDL_WriteBE32	=    SDL-WRITE-BE-32
;;;; SDLK_SLASH		=    :KEY-SLASH
;;;; SDLK_F1		=    :KEY-F-1
;;;; KMOD_LSHIFT	=    :KEY-MOD-LSHIFT



;;;; Overrides to C header files follow:
;;;;

;;;;
;;;; end Overrides


;;;; "SDL_mixer.h"

(defun SDL-MIXER-VERSION (x)
  (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) x sdl-cffi::sdl-version)
    (setf sdl-cffi::major SDL-MIXER-MAJOR-VERSION
          sdl-cffi::minor SDL-MIXER-MINOR-VERSION
          sdl-cffi::patch SDL-MIXER-PATCH-LEVEL)))

(defun MIX-VERSION (x)
  (SDL-MIXER-VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant MIX-DEFAULT-FORMAT sdl-cffi::AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant MIX-DEFAULT-FORMAT sdl-cffi::AUDIO-S16LSB) ;; Little Endian

(defun Mix-Load-WAV (file)
  (Mix-Load-WAV-RW (sdl-cffi::SDL-RW-FROM-FILE file "rb") 1))

(defun Mix-Play-Channel (channel chunk loops)
  (Mix-Play-Channel-Timed channel chunk loops -1))

(defun Mix-Fade-In-Channel (channel chunk loops ms)
  (Mix-Fade-In-Channel-Timed channel chunk loops ms -1))

(defun Mix-Get-Error ()
  (sdl-cffi::SDL-Get-Error))




;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant #.(openrm-lispify "SDL_MIXER_MAJOR_VERSION" 'constant) 1)

(cl:defconstant #.(openrm-lispify "SDL_MIXER_MINOR_VERSION" 'constant) 2)

(cl:defconstant #.(openrm-lispify "SDL_MIXER_PATCHLEVEL" 'constant) 8)

(cl:defconstant #.(openrm-lispify "MIX_MAJOR_VERSION" 'constant) 1)

(cl:defconstant #.(openrm-lispify "MIX_MINOR_VERSION" 'constant) 2)

(cl:defconstant #.(openrm-lispify "MIX_PATCHLEVEL" 'constant) 8)

(cffi:defcfun ("Mix_Linked_Version" #.(openrm-lispify "Mix_Linked_Version" 'function)) :pointer)

(cl:defconstant #.(openrm-lispify "MIX_CHANNELS" 'constant) 8)

(cl:defconstant #.(openrm-lispify "MIX_DEFAULT_FREQUENCY" 'constant) 22050)

(cl:defconstant #.(openrm-lispify "MIX_DEFAULT_CHANNELS" 'constant) 2)

(cl:defconstant #.(openrm-lispify "MIX_MAX_VOLUME" 'constant) 128)

(cffi:defcstruct #.(openrm-lispify "Mix_Chunk" 'classname)
	(#.(openrm-lispify "allocated" 'slotname) :int)
	(#.(openrm-lispify "abuf" 'slotname) :pointer)
	(#.(openrm-lispify "alen" 'slotname) :unsigned-int)
	(#.(openrm-lispify "volume" 'slotname) :unsigned-char))

(cffi:defcenum #.(openrm-lispify "Mix_Fading" 'enumname)
	#.(openrm-lispify "MIX_NO_FADING" 'enumvalue :keyword)
	#.(openrm-lispify "MIX_FADING_OUT" 'enumvalue :keyword)
	#.(openrm-lispify "MIX_FADING_IN" 'enumvalue :keyword))

(cffi:defcenum #.(openrm-lispify "Mix_MusicType" 'enumname)
	#.(openrm-lispify "MUS_NONE" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_CMD" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_WAV" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_MOD" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_MID" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_OGG" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_MP3" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_MP3_MAD" 'enumvalue :keyword)
	#.(openrm-lispify "MUS_FLAC" 'enumvalue :keyword))

(cffi:defcfun ("Mix_OpenAudio" #.(openrm-lispify "Mix_OpenAudio" 'function)) :int
  (frequency :int)
  (format :unsigned-short)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_AllocateChannels" #.(openrm-lispify "Mix_AllocateChannels" 'function)) :int
  (numchans :int))

(cffi:defcfun ("Mix_QuerySpec" #.(openrm-lispify "Mix_QuerySpec" 'function)) :int
  (frequency :pointer)
  (format :pointer)
  (channels :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" #.(openrm-lispify "Mix_LoadWAV_RW" 'function)) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("Mix_LoadMUS" #.(openrm-lispify "Mix_LoadMUS" 'function)) :pointer
  (file :string))

(cffi:defcfun ("Mix_LoadMUS_RW" #.(openrm-lispify "Mix_LoadMUS_RW" 'function)) :pointer
  (rw :pointer))

(cffi:defcfun ("Mix_QuickLoad_WAV" #.(openrm-lispify "Mix_QuickLoad_WAV" 'function)) :pointer
  (mem :pointer))

(cffi:defcfun ("Mix_QuickLoad_RAW" #.(openrm-lispify "Mix_QuickLoad_RAW" 'function)) :pointer
  (mem :pointer)
  (len :unsigned-int))

(cffi:defcfun ("Mix_FreeChunk" #.(openrm-lispify "Mix_FreeChunk" 'function)) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" #.(openrm-lispify "Mix_FreeMusic" 'function)) :void
  (music :pointer))

(cffi:defcfun ("Mix_GetMusicType" #.(openrm-lispify "Mix_GetMusicType" 'function)) #.(openrm-lispify "Mix_MusicType" 'enumname)
  (music :pointer))

(cffi:defcfun ("Mix_SetPostMix" #.(openrm-lispify "Mix_SetPostMix" 'function)) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusic" #.(openrm-lispify "Mix_HookMusic" 'function)) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusicFinished" #.(openrm-lispify "Mix_HookMusicFinished" 'function)) :void
  (music_finished :pointer))

(cffi:defcfun ("Mix_GetMusicHookData" #.(openrm-lispify "Mix_GetMusicHookData" 'function)) :pointer)

(cffi:defcfun ("Mix_ChannelFinished" #.(openrm-lispify "Mix_ChannelFinished" 'function)) :void
  (channel_finished :pointer))

(cl:defconstant #.(openrm-lispify "MIX_CHANNEL_POST" 'constant) -2)

(cffi:defcfun ("Mix_RegisterEffect" #.(openrm-lispify "Mix_RegisterEffect" 'function)) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_UnregisterEffect" #.(openrm-lispify "Mix_UnregisterEffect" 'function)) :int
  (channel :int)
  (f :pointer))

(cffi:defcfun ("Mix_UnregisterAllEffects" #.(openrm-lispify "Mix_UnregisterAllEffects" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_SetPanning" #.(openrm-lispify "Mix_SetPanning" 'function)) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))

(cffi:defcfun ("Mix_SetPosition" #.(openrm-lispify "Mix_SetPosition" 'function)) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetDistance" #.(openrm-lispify "Mix_SetDistance" 'function)) :int
  (channel :int)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetReverseStereo" #.(openrm-lispify "Mix_SetReverseStereo" 'function)) :int
  (channel :int)
  (flip :int))

(cffi:defcfun ("Mix_ReserveChannels" #.(openrm-lispify "Mix_ReserveChannels" 'function)) :int
  (num :int))

(cffi:defcfun ("Mix_GroupChannel" #.(openrm-lispify "Mix_GroupChannel" 'function)) :int
  (which :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupChannels" #.(openrm-lispify "Mix_GroupChannels" 'function)) :int
  (from :int)
  (to :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupAvailable" #.(openrm-lispify "Mix_GroupAvailable" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupCount" #.(openrm-lispify "Mix_GroupCount" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupOldest" #.(openrm-lispify "Mix_GroupOldest" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupNewer" #.(openrm-lispify "Mix_GroupNewer" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_PlayChannelTimed" #.(openrm-lispify "Mix_PlayChannelTimed" 'function)) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ticks :int))

(cffi:defcfun ("Mix_PlayMusic" #.(openrm-lispify "Mix_PlayMusic" 'function)) :int
  (music :pointer)
  (loops :int))

(cffi:defcfun ("Mix_FadeInMusic" #.(openrm-lispify "Mix_FadeInMusic" 'function)) :int
  (music :pointer)
  (loops :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeInMusicPos" #.(openrm-lispify "Mix_FadeInMusicPos" 'function)) :int
  (music :pointer)
  (loops :int)
  (ms :int)
  (position :double))

(cffi:defcfun ("Mix_FadeInChannelTimed" #.(openrm-lispify "Mix_FadeInChannelTimed" 'function)) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ms :int)
  (ticks :int))

(cffi:defcfun ("Mix_Volume" #.(openrm-lispify "Mix_Volume" 'function)) :int
  (channel :int)
  (volume :int))

(cffi:defcfun ("Mix_VolumeChunk" #.(openrm-lispify "Mix_VolumeChunk" 'function)) :int
  (chunk :pointer)
  (volume :int))

(cffi:defcfun ("Mix_VolumeMusic" #.(openrm-lispify "Mix_VolumeMusic" 'function)) :int
  (volume :int))

(cffi:defcfun ("Mix_HaltChannel" #.(openrm-lispify "Mix_HaltChannel" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_HaltGroup" #.(openrm-lispify "Mix_HaltGroup" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_HaltMusic" #.(openrm-lispify "Mix_HaltMusic" 'function)) :int)

(cffi:defcfun ("Mix_ExpireChannel" #.(openrm-lispify "Mix_ExpireChannel" 'function)) :int
  (channel :int)
  (ticks :int))

(cffi:defcfun ("Mix_FadeOutChannel" #.(openrm-lispify "Mix_FadeOutChannel" 'function)) :int
  (which :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutGroup" #.(openrm-lispify "Mix_FadeOutGroup" 'function)) :int
  (tag :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutMusic" #.(openrm-lispify "Mix_FadeOutMusic" 'function)) :int
  (ms :int))

(cffi:defcfun ("Mix_FadingMusic" #.(openrm-lispify "Mix_FadingMusic" 'function)) #.(openrm-lispify "Mix_Fading" 'enumname))

(cffi:defcfun ("Mix_FadingChannel" #.(openrm-lispify "Mix_FadingChannel" 'function)) #.(openrm-lispify "Mix_Fading" 'enumname)
  (which :int))

(cffi:defcfun ("Mix_Pause" #.(openrm-lispify "Mix_Pause" 'function)) :void
  (channel :int))

(cffi:defcfun ("Mix_Resume" #.(openrm-lispify "Mix_Resume" 'function)) :void
  (channel :int))

(cffi:defcfun ("Mix_Paused" #.(openrm-lispify "Mix_Paused" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_PauseMusic" #.(openrm-lispify "Mix_PauseMusic" 'function)) :void)

(cffi:defcfun ("Mix_ResumeMusic" #.(openrm-lispify "Mix_ResumeMusic" 'function)) :void)

(cffi:defcfun ("Mix_RewindMusic" #.(openrm-lispify "Mix_RewindMusic" 'function)) :void)

(cffi:defcfun ("Mix_PausedMusic" #.(openrm-lispify "Mix_PausedMusic" 'function)) :int)

(cffi:defcfun ("Mix_SetMusicPosition" #.(openrm-lispify "Mix_SetMusicPosition" 'function)) :int
  (position :double))

(cffi:defcfun ("Mix_Playing" #.(openrm-lispify "Mix_Playing" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_PlayingMusic" #.(openrm-lispify "Mix_PlayingMusic" 'function)) :int)

(cffi:defcfun ("Mix_SetMusicCMD" #.(openrm-lispify "Mix_SetMusicCMD" 'function)) :int
  (command :string))

(cffi:defcfun ("Mix_SetSynchroValue" #.(openrm-lispify "Mix_SetSynchroValue" 'function)) :int
  (value :int))

(cffi:defcfun ("Mix_GetSynchroValue" #.(openrm-lispify "Mix_GetSynchroValue" 'function)) :int)

(cffi:defcfun ("Mix_GetChunk" #.(openrm-lispify "Mix_GetChunk" 'function)) :pointer
  (channel :int))

(cffi:defcfun ("Mix_CloseAudio" #.(openrm-lispify "Mix_CloseAudio" 'function)) :void)


