
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
  (cl:unless (cl:fboundp 'sdlmixer-lispify)
    (defun sdlmixer-lispify (name flag &optional (package *package*))
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
	       (remove-sub (old lis)
		 (nthcdr (length old) lis))
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
		    (let ((old '((#\M #\i #\x #\_) (#\M #\I #\X #\_))))
		      (dolist (str old)
			(when (find-sub str lst)
      			(setf lst (remove-sub str lst)
			      c (first lst)))))
		    (helper (cdr lst) 'upper last
			    (cond
			      ((or (equal last 'lower)
				   ;; (equal last 'digit)
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
	   package))))))

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

;;;; These defctypes are used by the CFFI translation functions
;;;; see the typemap definition below.
(cffi:defctype mix-chunk-fp (:wrapper :pointer 
				      :to-c to-mix-chunk))
(cffi:defctype mix-music-fp (:wrapper :pointer 
				      :to-c to-mix-music))



;;;; Overrides to C header files follow:
;;;;

;;;;
;;;; end Overrides


;;;; "SDL_mixer.h"

(cffi:defcfun ("Mix_FreeChunk" FREE-CHUNK) :void
  (chunk :pointer))

(cffi:defcfun ("Mix_FreeMusic" FREE-MUSIC) :void
  (music :pointer))

(defun SDL-MIXER-VERSION (sdl-version)
  (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) sdl-version sdl-cffi::sdl-version)
    (setf sdl-cffi::major +SDL-MIXER-MAJOR-VERSION+
          sdl-cffi::minor +SDL-MIXER-MINOR-VERSION+
          sdl-cffi::patch +SDL-MIXER-PATCHLEVEL+)))

(defun VERSION (x)
  (SDL-MIXER-VERSION x))

#-(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant +DEFAULT-FORMAT+ sdl-cffi::AUDIO-S16LSB) ;; Little Endian

(defun Load-WAV (file)
  (Load-WAV-RW (sdl-cffi::SDL-RW-FROM-FILE file "rb") 1))

(defun Play-Channel (channel chunk loops)
  (Play-Channel-Timed channel chunk loops -1))

(defun Fade-In-Channel (channel chunk loops ms)
  (Fade-In-Channel-Timed channel chunk loops ms -1))

(defun Get-Error ()
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


(cl:defconstant #.(sdlmixer-lispify "SDL_MIXER_MAJOR_VERSION" 'constant) 1)

(cl:defconstant #.(sdlmixer-lispify "SDL_MIXER_MINOR_VERSION" 'constant) 2)

(cl:defconstant #.(sdlmixer-lispify "SDL_MIXER_PATCHLEVEL" 'constant) 8)

(cl:defconstant #.(sdlmixer-lispify "MIX_MAJOR_VERSION" 'constant) 1)

(cl:defconstant #.(sdlmixer-lispify "MIX_MINOR_VERSION" 'constant) 2)

(cl:defconstant #.(sdlmixer-lispify "MIX_PATCHLEVEL" 'constant) 8)

(cffi:defcfun ("Mix_Linked_Version" #.(sdlmixer-lispify "Mix_Linked_Version" 'function)) :pointer)

(cl:defconstant #.(sdlmixer-lispify "MIX_CHANNELS" 'constant) 8)

(cl:defconstant #.(sdlmixer-lispify "MIX_DEFAULT_FREQUENCY" 'constant) 22050)

(cl:defconstant #.(sdlmixer-lispify "MIX_DEFAULT_CHANNELS" 'constant) 2)

(cl:defconstant #.(sdlmixer-lispify "MIX_MAX_VOLUME" 'constant) 128)

(cffi:defcstruct #.(sdlmixer-lispify "Mix_Chunk" 'classname)
	(#.(sdlmixer-lispify "allocated" 'slotname) :int)
	(#.(sdlmixer-lispify "abuf" 'slotname) :pointer)
	(#.(sdlmixer-lispify "alen" 'slotname) :unsigned-int)
	(#.(sdlmixer-lispify "volume" 'slotname) :unsigned-char))

(cffi:defcenum #.(sdlmixer-lispify "Mix_Fading" 'enumname)
	#.(sdlmixer-lispify "MIX_NO_FADING" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MIX_FADING_OUT" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MIX_FADING_IN" 'enumvalue :keyword))

(cffi:defcenum #.(sdlmixer-lispify "Mix_MusicType" 'enumname)
	#.(sdlmixer-lispify "MUS_NONE" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_CMD" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_WAV" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_MOD" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_MID" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_OGG" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_MP3" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_MP3_MAD" 'enumvalue :keyword)
	#.(sdlmixer-lispify "MUS_FLAC" 'enumvalue :keyword))

(cffi:defcfun ("Mix_OpenAudio" #.(sdlmixer-lispify "Mix_OpenAudio" 'function)) :int
  (frequency :int)
  (format :unsigned-short)
  (channels :int)
  (chunksize :int))

(cffi:defcfun ("Mix_AllocateChannels" #.(sdlmixer-lispify "Mix_AllocateChannels" 'function)) :int
  (numchans :int))

(cffi:defcfun ("Mix_QuerySpec" #.(sdlmixer-lispify "Mix_QuerySpec" 'function)) :int
  (frequency :pointer)
  (format :pointer)
  (channels :pointer))

(cffi:defcfun ("Mix_LoadWAV_RW" #.(sdlmixer-lispify "Mix_LoadWAV_RW" 'function)) :pointer
  (src :pointer)
  (freesrc :int))

(cffi:defcfun ("Mix_LoadMUS" #.(sdlmixer-lispify "Mix_LoadMUS" 'function)) :pointer
  (file :string))

(cffi:defcfun ("Mix_LoadMUS_RW" #.(sdlmixer-lispify "Mix_LoadMUS_RW" 'function)) :pointer
  (rw :pointer))

(cffi:defcfun ("Mix_QuickLoad_WAV" #.(sdlmixer-lispify "Mix_QuickLoad_WAV" 'function)) :pointer
  (mem :pointer))

(cffi:defcfun ("Mix_QuickLoad_RAW" #.(sdlmixer-lispify "Mix_QuickLoad_RAW" 'function)) :pointer
  (mem :pointer)
  (len :unsigned-int))

(cffi:defcfun ("Mix_GetMusicType" #.(sdlmixer-lispify "Mix_GetMusicType" 'function)) #.(sdlmixer-lispify "Mix_MusicType" 'enumname)
  (music mix-music-fp))

(cffi:defcfun ("Mix_SetPostMix" #.(sdlmixer-lispify "Mix_SetPostMix" 'function)) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusic" #.(sdlmixer-lispify "Mix_HookMusic" 'function)) :void
  (mix_func :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_HookMusicFinished" #.(sdlmixer-lispify "Mix_HookMusicFinished" 'function)) :void
  (music_finished :pointer))

(cffi:defcfun ("Mix_GetMusicHookData" #.(sdlmixer-lispify "Mix_GetMusicHookData" 'function)) :pointer)

(cffi:defcfun ("Mix_ChannelFinished" #.(sdlmixer-lispify "Mix_ChannelFinished" 'function)) :void
  (channel_finished :pointer))

(cl:defconstant #.(sdlmixer-lispify "MIX_CHANNEL_POST" 'constant) -2)

(cffi:defcfun ("Mix_RegisterEffect" #.(sdlmixer-lispify "Mix_RegisterEffect" 'function)) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))

(cffi:defcfun ("Mix_UnregisterEffect" #.(sdlmixer-lispify "Mix_UnregisterEffect" 'function)) :int
  (channel :int)
  (f :pointer))

(cffi:defcfun ("Mix_UnregisterAllEffects" #.(sdlmixer-lispify "Mix_UnregisterAllEffects" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_SetPanning" #.(sdlmixer-lispify "Mix_SetPanning" 'function)) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))

(cffi:defcfun ("Mix_SetPosition" #.(sdlmixer-lispify "Mix_SetPosition" 'function)) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetDistance" #.(sdlmixer-lispify "Mix_SetDistance" 'function)) :int
  (channel :int)
  (distance :unsigned-char))

(cffi:defcfun ("Mix_SetReverseStereo" #.(sdlmixer-lispify "Mix_SetReverseStereo" 'function)) :int
  (channel :int)
  (flip :int))

(cffi:defcfun ("Mix_ReserveChannels" #.(sdlmixer-lispify "Mix_ReserveChannels" 'function)) :int
  (num :int))

(cffi:defcfun ("Mix_GroupChannel" #.(sdlmixer-lispify "Mix_GroupChannel" 'function)) :int
  (which :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupChannels" #.(sdlmixer-lispify "Mix_GroupChannels" 'function)) :int
  (from :int)
  (to :int)
  (tag :int))

(cffi:defcfun ("Mix_GroupAvailable" #.(sdlmixer-lispify "Mix_GroupAvailable" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupCount" #.(sdlmixer-lispify "Mix_GroupCount" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupOldest" #.(sdlmixer-lispify "Mix_GroupOldest" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_GroupNewer" #.(sdlmixer-lispify "Mix_GroupNewer" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_PlayChannelTimed" #.(sdlmixer-lispify "Mix_PlayChannelTimed" 'function)) :int
  (channel :int)
  (chunk mix-chunk-fp)
  (loops :int)
  (ticks :int))

(cffi:defcfun ("Mix_PlayMusic" #.(sdlmixer-lispify "Mix_PlayMusic" 'function)) :int
  (music mix-music-fp)
  (loops :int))

(cffi:defcfun ("Mix_FadeInMusic" #.(sdlmixer-lispify "Mix_FadeInMusic" 'function)) :int
  (music mix-music-fp)
  (loops :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeInMusicPos" #.(sdlmixer-lispify "Mix_FadeInMusicPos" 'function)) :int
  (music mix-music-fp)
  (loops :int)
  (ms :int)
  (position :double))

(cffi:defcfun ("Mix_FadeInChannelTimed" #.(sdlmixer-lispify "Mix_FadeInChannelTimed" 'function)) :int
  (channel :int)
  (chunk mix-chunk-fp)
  (loops :int)
  (ms :int)
  (ticks :int))

(cffi:defcfun ("Mix_Volume" #.(sdlmixer-lispify "Mix_Volume" 'function)) :int
  (channel :int)
  (volume :int))

(cffi:defcfun ("Mix_VolumeChunk" #.(sdlmixer-lispify "Mix_VolumeChunk" 'function)) :int
  (chunk mix-chunk-fp)
  (volume :int))

(cffi:defcfun ("Mix_VolumeMusic" #.(sdlmixer-lispify "Mix_VolumeMusic" 'function)) :int
  (volume :int))

(cffi:defcfun ("Mix_HaltChannel" #.(sdlmixer-lispify "Mix_HaltChannel" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_HaltGroup" #.(sdlmixer-lispify "Mix_HaltGroup" 'function)) :int
  (tag :int))

(cffi:defcfun ("Mix_HaltMusic" #.(sdlmixer-lispify "Mix_HaltMusic" 'function)) :int)

(cffi:defcfun ("Mix_ExpireChannel" #.(sdlmixer-lispify "Mix_ExpireChannel" 'function)) :int
  (channel :int)
  (ticks :int))

(cffi:defcfun ("Mix_FadeOutChannel" #.(sdlmixer-lispify "Mix_FadeOutChannel" 'function)) :int
  (which :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutGroup" #.(sdlmixer-lispify "Mix_FadeOutGroup" 'function)) :int
  (tag :int)
  (ms :int))

(cffi:defcfun ("Mix_FadeOutMusic" #.(sdlmixer-lispify "Mix_FadeOutMusic" 'function)) :int
  (ms :int))

(cffi:defcfun ("Mix_FadingMusic" #.(sdlmixer-lispify "Mix_FadingMusic" 'function)) #.(sdlmixer-lispify "Mix_Fading" 'enumname))

(cffi:defcfun ("Mix_FadingChannel" #.(sdlmixer-lispify "Mix_FadingChannel" 'function)) #.(sdlmixer-lispify "Mix_Fading" 'enumname)
  (which :int))

(cffi:defcfun ("Mix_Pause" #.(sdlmixer-lispify "Mix_Pause" 'function)) :void
  (channel :int))

(cffi:defcfun ("Mix_Resume" #.(sdlmixer-lispify "Mix_Resume" 'function)) :void
  (channel :int))

(cffi:defcfun ("Mix_Paused" #.(sdlmixer-lispify "Mix_Paused" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_PauseMusic" #.(sdlmixer-lispify "Mix_PauseMusic" 'function)) :void)

(cffi:defcfun ("Mix_ResumeMusic" #.(sdlmixer-lispify "Mix_ResumeMusic" 'function)) :void)

(cffi:defcfun ("Mix_RewindMusic" #.(sdlmixer-lispify "Mix_RewindMusic" 'function)) :void)

(cffi:defcfun ("Mix_PausedMusic" #.(sdlmixer-lispify "Mix_PausedMusic" 'function)) :int)

(cffi:defcfun ("Mix_SetMusicPosition" #.(sdlmixer-lispify "Mix_SetMusicPosition" 'function)) :int
  (position :double))

(cffi:defcfun ("Mix_Playing" #.(sdlmixer-lispify "Mix_Playing" 'function)) :int
  (channel :int))

(cffi:defcfun ("Mix_PlayingMusic" #.(sdlmixer-lispify "Mix_PlayingMusic" 'function)) :int)

(cffi:defcfun ("Mix_SetMusicCMD" #.(sdlmixer-lispify "Mix_SetMusicCMD" 'function)) :int
  (command :string))

(cffi:defcfun ("Mix_SetSynchroValue" #.(sdlmixer-lispify "Mix_SetSynchroValue" 'function)) :int
  (value :int))

(cffi:defcfun ("Mix_GetSynchroValue" #.(sdlmixer-lispify "Mix_GetSynchroValue" 'function)) :int)

(cffi:defcfun ("Mix_GetChunk" #.(sdlmixer-lispify "Mix_GetChunk" 'function)) :pointer
  (channel :int))

(cffi:defcfun ("Mix_CloseAudio" #.(sdlmixer-lispify "Mix_CloseAudio" 'function)) :void)


