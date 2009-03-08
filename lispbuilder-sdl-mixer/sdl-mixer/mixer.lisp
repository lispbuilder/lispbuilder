
(in-package #:sdl-mixer)


;; Configure Lispworks to allow
;; callbacks from foreign threads
#+(and lispworks (not lispworks5.1)) (system:setup-for-alien-threads)

;;;; Add documentation strings for defconstants in cffi/sdl-mixer.lisp
(setf (documentation '+default-format+ 'variable)
      "Default SDL audio format; little-endian is `SDL:AUDIO-S16LSB`, big-endian is `SDL:AUDIO-S16MSB`.
Audio formats are defined in `SDL_audio.h`;

* `SDL:AUDIO-U8`     : Unsigned 8-bit samples
* `SDL:AUDIO-S8`     : Signed 8-bit samples
* `SDL:AUDIO-U16LSB` : Unsigned 16-bit samples, in little-endian byte order
* `SDL:AUDIO-S16LSB` : Signed 16-bit samples, in little-endian byte order
* `SDL:AUDIO-U16MSB` : Unsigned 16-bit samples, in big-endian byte order
* `SDL:AUDIO-S16MSB` : Signed 16-bit samples, in big-endian byte order
* `SDL:AUDIO-U16`    : same as `SDL:AUDIO-U16LSB` (for backwards compatability probably)
* `SDL:AUDIO-S16`    : same as `SDL:AUDIO-S16LSB` (for backwards compatability probably)
* `SDL:AUDIO-U16SYS` : Unsigned 16-bit samples, in system byte order
* `SDL:AUDIO-S16SYS` : Signed 16-bit samples, in system byte order")


(setf (documentation 'sdl-mixer-version 'function)
      "Sets the `SDL-VERSION` structure with the version of the library..")
(setf (documentation '+channels+ 'variable)
      "Default number of `8` mixer channels.")
(setf (documentation '+default-frequency+ 'variable)
      "Default sampling frequency of `22,050hz`")
(setf (documentation '+default-channels+ 'variable)
      "Default number of `2` sound channels for Stereo output.")
(setf (documentation '+max-volume+ 'variable)
      "Default volume of `128` for [CHUNK](#chunk), output channels and mix channels.")
(setf (documentation '+channel-post+ 'variable)
      "Default channel of `-2` used for post-processing.")

(setf (documentation 'chunk 'standard-class)
      "A new `CHUNK` object is returned by [LOAD-SAMPLE](#load-sample) and [sample-FROM-CHANNEL](#sample-from-channel).
A `CHUNK` object wraps a reference to the foreign buffer containing the `SAMPLE` data.
The sample referenced by the `CHUNK` can be freed by calling [FREE](#free). 
Do not free a `CHUNK` while the `SAMPLE` is playing.
Do not attempt to use a `CHUNK` after it is freed.")

(setf (documentation 'music 'standard-class)
      "A new `MUSIC` object is returned by [LOAD-MUSIC](#load-music).
A `MUSIC` object wraps a reference to the foreign buffer containing the `MUSIC` data.
The sample referenced by `MUSIC` can be freed by calling [FREE](#free). 
Do not free `MUSIC` when playing.
Do not attempt to use `MUSIC` after it is freed.")

(defconstant +default-sample-buffer+ 4096
  "Default size of the sample output buffer is `4096` bytes")

(defun reserve-channels (channels)
  "Reserves, or excludes, the number of `CHANNELS` from default mixing. 
The number of channels to reserve is from 0 to `\(- CHANNELS 1\)`.
Default mixing is performed when when a sample is played on an unreserved channel, 
when using [PLAY-SAMPLE](#play-sample) with `:CHANNEL` `NIL`.
`CHANNEL` when `NIL` or `0` will remove all reservations.
Channels are unreserved unless explicitly reserved by [RESERVE-CHANNELS](#reserve-channels).
Returns the number of channels reserved. May return less channels than requested, depending on the 
number of channels previously allocated using [ALLOCATE-CHANNELS](#allocate-channels)."
  (sdl-mixer-cffi::reserve-channels (if (integerp channels) channels 0)))

(defmethod load-music ((filepath STRING))
  "Loads the music file at location `FILEPATH`. Must be a `WAVE`, `MOD`, `MIDI`, `OGG` or `MP3` file.
Returns music as a new [MUSIC](#music) object, or NIL on error."
  (let ((file (namestring filepath)))
    (if (and (stringp file) (probe-file file))
	(let ((music-fp (sdl-mixer-cffi::LOAD-MUS file)))
	  (if (sdl-base:is-valid-ptr music-fp)
	      (make-instance 'sdl-mixer-cffi::music :fp music-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))

(defmethod load-music ((rwops SDL:RWOPS))
  "Loads the music file from `RWOPS`. Must be a `WAVE`, `MOD`, `MIDI`, `OGG` or `MP3` file.
Returns music as a new [MUSIC](#music) object, or NIL on error."
  (let ((music-fp (sdl-mixer-cffi::LOAD-MUS-RW (sdl:fp rwops))))
    (if (sdl-base:is-valid-ptr music-fp)
	(make-instance 'sdl-mixer-cffi::music :fp music-fp)
	(error "Cannot load music from rwops ~A." rwops))))

(defmethod load-music ((array VECTOR))
  "Loads the music file from the byte `ARRAY`. Must be a `WAVE`, `MOD`, `MIDI`, `OGG` or `MP3` file.
Returns music as a new [MUSIC](#music) object and a new `RWOPS` object.
Maintain references to both of these objects until the music can be freed."
  (let ((rwops (sdl::create-rwops-from-byte-array array)))
    (values (load-music rwops)
	    rwops)))

(defmethod load-sample ((filepath STRING))
  "Loads the sample file at location `FILEPATH`. Must be a `WAVE`, `AIFF`, `RIFF`, `OGG`, or `VOC` file.
Returns the sample as a new [CHUNK](#chunk) object, or NIL on error."
  (let ((file (namestring filepath)))
    (if (and (stringp file) (probe-file file))
      (let ((chunk-fp (sdl-mixer-cffi::LOAD-WAV file)))
        (if (sdl-base:is-valid-ptr chunk-fp)
          (make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp)
          (error "Cannot load ~A." file)))
      (error "Sample file ~A does not exist." file))))

(defmethod load-sample ((rwops sdl:RWOPS))
  "Loads the sample from `RWOPS`. Must be a `WAVE`, `AIFF`, `RIFF`, `OGG`, or `VOC` file.
Returns the sample as a new [CHUNK](#chunk) object, or NIL on error."
  (let ((chunk-fp (sdl-mixer-cffi::LOAD-WAV-RW (sdl:fp rwops) 0)))
    (if (sdl-base:is-valid-ptr chunk-fp)
	(make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp)
	(error "Cannot load sample from rwops ~A." rwops))))

(defmethod load-sample ((array VECTOR))
  "Loads the sample file from the byte `ARRAY`. Must be a `WAVE`, `AIFF`, `RIFF`, `OGG`, or `VOC` file.
Returns the sample as a new [CHUNK](#chunk) object, or NIL on error."
  (let* ((rwops (sdl::create-rwops-from-byte-array array))
	 (chunk (load-sample rwops)))
    (sdl:free rwops)
    chunk))

(defun sample-from-channel (channel)
  "Returns currently playing or most recently played sample on `CHANNEL` as a new [CHUNK](#chunk) object, 
or `NIL` if `CHANNEL` is not allocated or `CHANNEL` has not yet played out any samples.
NOTE: The sample may already have been freed and therefore the pointer to the foreign object in [CHUNK](#chunk) may not be valid."
  (let ((chunk-fp (sdl-mixer-cffi::get-chunk channel)))
    (if (sdl-base:is-valid-ptr chunk-fp)
      (make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp :gc nil)
      nil)))

(defun open-audio (&key
                   (frequency +DEFAULT-FREQUENCY+)
		   (format +DEFAULT-FORMAT+)
		   (channels +DEFAULT-CHANNELS+)
		   (chunksize +DEFAULT-SAMPLE-BUFFER+))
  "Initializes the mixer. SDL must be initialized with [SDL-INIT-AUDIO](#sdl-init-audio) prior to this call. 
[OPEN-AUDIO](#open-audio) can be called multiple times, however `FORMAT` is set on the first call and will not changed on subsequent calls. 
The audio device must be closed and re-opened for any change to `FORMAT` to take effect.
[CLOSE-AUDIO](#close-audio) must be called the same number of time to close the audio device.
Use [ALLOCATE-CHANNELS](#allocate-channels) to set the number of mixing channels. [OPEN-AUDIO](#open-audio) will allocate [+MIX-CHANNELS+](#+channels+) by default. 

##### Parameters

* `FREQUENCY` is the output sampling frequency in samples per second (Hz). Default is [+DEFAULT-FREQUENCY+](#+default-frequency+).
Most games use a `FREQUENCY` of [+DEFAULT-FREQUENCY+](#+default-frequency+). The higher the `FREQUENCY` the greater the CPU resource requirements. 
\(A value of 44100 is the CD audio rate\).
* `FORMAT` is the output sample format. Default is [+DEFAULT-FORMAT+](#+default-format+).
* `CHANNELS` sets the number of sound channels in the output. Default is [+DEFAULT-CHANNELS+](#+default-channels+). 
This is not the same as [ALLOCATE-CHANNELS](#allocate-channels) which sets the number of mixing channels.
* `CHUNKSIZE` is the size in bytes of each mixed sample buffer. Default is [+DEFAULT-SAMPLE-BUFFER+](#+default-sample-buffer+) bytes.
The smaller the `CHUNKSIZE`, the more frequenctly the mixer hooks are called. 
Increase `CHUNKSIZE` to decrease CPU resources, if sound skips or if playing music. Decrease `CHUNKSIZE` to decrease sound lag.

##### Returns

* `T` on success and `NIL` on failure."
  ;; Make sure that SDL is initialized with SDL:SDL-INIT-AUDIO
  (when (= 0 (sdl:return-subsystems-of-status SDL:SDL-INIT-AUDIO t))
    (error "OPEN-AUDIO: SDL must be initialized with SDL:SDL-INIT-AUDIO prior to calling OPEN-AUDIO."))
  (if (= 0 (sdl-mixer-cffi::open-audio frequency format channels chunksize))
    (progn
      ;; Register the music finished callback
      ;(register-music-finished nil)
      ;; Register the sample finished callback
      ;(register-sample-finished nil)
      t)
    nil))

(defun close-audio (&optional (all nil))
  "Attempts to close the audio device. The audio device can be opened multiple times by [OPEN-AUDIO](#open-audio) 
and to properly close the audio device, [CLOSE-AUDIO](#close-audio) should be called the same number of times. 
Optionally `ALL` when `T` will forcibly close the audio device, no matter how many times the device was opened."
  ;; Unregister the music finished callback
  ;(register-music-finished nil)
  ;; Unregister the sample finished callback
  ;(register-sample-finished nil)
  (format t "audio-opened-p: ~A,  query-spec: ~A~%" (audio-opened-p) (query-spec))
  (if (and all (audio-opened-p))
    (dotimes (i (query-spec))
      (sdl-mixer-cffi::close-audio))
    (sdl-mixer-cffi::close-audio)))

(defun play-music (music &key
		   (loop nil)
		   (fade nil)
		   (position 0))
  "Starts playing [MUSIC](#music) from `POSITION`. The music can be faded-in over the number of milliseconds in `FADE`.
Automatically repeats the music when finished the number of time specified in `LOOP`. 
Any previous music will be halted. Will block until any current fade effect completes.

##### Parameters

* `MUSIC` is the [MUSIC](#music) to play.
* `LOOP` is the number of times to play the music. T continuously loops the music. Plays once when `NIL`, or `0`. Default is `NIL`.
* `FADE` is the number of milliseconds to perform the fade-in effect. Default is `NIL`, no fade effect. Only applies to the first loop.
* `POSITION` is the position in the music to begin playing from. Default is to start playout from the beginning.

##### Returns

* T on success and NIL on failure."
  (cond
    ((eq loop t)
     (setf loop -1))
    ((null loop)
     (setf loop 0)))
  (if (integerp fade)
      (if (= 1 (sdl-mixer-cffi::fade-in-music-pos music loop fade position))
	  t
	  nil)
      (if (= 1 (sdl-mixer-cffi::play-music music loop))
	  t
	  nil)))

(defun play-sample (chunk &key
		    (channel t)
		    (group nil)
		    (loop nil)
		    (fade nil)
		    (ticks t))
  "Plays the sample in [CHUNK](#chunk) for `TICKS` milliseconds, repeated `LOOP` times.
The sample is repeated the number of times specified in `LOOP`, or until `TICKS`. 
The sample will fade-in over `FADE` milliseconds. 
The callback set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished) is called when the sample stops playing.

##### Parameters

* `CHUNK` is the sample [CHUNK](#chunk).
* `CHANNEL` specifies the channel to play the sample. When `T`, will perform default mixing and mix the 
sample on the first free unreserved channel.
* `GROUP` Attempts to play the sample on the first available \(not playing\) reserved channel in `GROUP`. 
When `T`, will search for the first available \(not playing\) reserved channel in all groups. 
If there are no available reserved channels then [PLAY-SAMPLE](#play-sample) will fail.
If however `CHANNEL` is also `T` then the default action is to play the sample on the first unreserved channel.
Use [RESERVE-CHANNELS](#reserve-channels) to reserve, or exclude, a channel from default mixing.
* `LOOP` is the number of times to loop the sample. Loops continuously when `T`. Plays once when `NIL`, or `0`. Default in `NIL`.
* `FADE` is the number of milliseconds to perform the fade-in effect. Default is `NIL`, no fade effect. Only applies to the first loop.
* `TICKS` is the number of milliseconds to play the sample. When `T` will play the sample from start to finish. Default is `T`.

##### Returns

* The channel the sample is playing on, or `NIL` on failure."
  (cond
    ((eq loop t)
     (setf loop -1))
    ((null loop)
     (setf loop 0)))
  (cond
    ((eq ticks t)
     (setf ticks -1))
    ((eq ticks nil)
     (setf ticks 0)))

  (if group
      (let ((channel-available (group-available group)))
	(if channel-available
	    (setf channel channel-available))))

  (if (integerp fade)
      (let ((chan (sdl-mixer-cffi::fade-in-channel-timed (if (integerp channel) channel -1) chunk loop fade ticks)))
	(if (= -1 chan)
	    nil
	    chan))
      (let ((chan (sdl-mixer-cffi::play-channel-timed (if (integerp channel) channel -1) chunk loop ticks)))
	(if (= -1 chan)
	    nil
	    chan))))

(defun halt-music (&optional (fade nil))
  "Stops playing music. The callback set by [REGISTER-MUSIC-FINISHED](#register-music-finished) is called when the music stops.

##### Parameters

* `FADE` is the number of milliseconds to perform the fade-out effect. Will block until any current fade effect completes. Has no effect on paused or halted music.
When `FADE` is NIL the music is stopped and all fade effects are immediately cancelled.
The default is NIL.

##### Returns

* T on success and NIL on failure."
  (if (integerp fade)
      (if (= 1 (sdl-mixer-cffi::fade-out-music fade))
	  t
	  nil)
      (progn
	(sdl-mixer-cffi::halt-music)
	t)))

(defun halt-sample (&key
		    (channel t)
		    (group nil)
		    (fade nil)
		    (ticks nil))
  "Stops playing the sample on `CHANNEL`, or stops playback on the reserved channels in `GROUP`. 
The callback set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished) is called when the channel stops.

##### Parameters

* `CHANNEL` specifies the channel to stop playing. When `T`, will stop playback samples on all channels. 
Ignores channels not currently playing out samples.
* `GROUP` specifies the group of reserved channels to stop playing. Use [RESERVE-CHANNELS](#reserve-channels) to create a group of 
reserved channels. 
* `FADE` is the number of milliseconds to perform the fade-out effect. 
When `CHANNEL` is set, will fade out the sample on the specified channel.
When `GROUP` is set, will fade out the samples on the reserved channels in `GROUP`. The sample is stopped when fade effect completes.
When `FADE` is `NIL` or `0` the fade effect is immediate. Default is `NIL`.
* `TICKS` is the number of milliseconds until the sample is stopped. When `NIL` or `0`, the sample is stopped immediately. Default is `NIL`.

##### Returns

* The number of samples fading out, when `FADE` is set.
* The number of samples halted, when `TICKS` is set.
* Otherwise, returns T on success and NIL on failure."
  (if (and (integerp fade) (integerp group))
      (sdl-mixer-cffi::fade-out-group group fade)
      (if (integerp group)
	  (progn
	    (sdl-mixer-cffi::halt-group group)
	    t)
	  (progn
	    (if (or (eq channel t) (null channel))
		(setf channel -1))
	    (if (and (null fade) (null ticks))
		(progn
		  (sdl-mixer-cffi::halt-channel channel)
		  t)
		(if (integerp fade)
		    (sdl-mixer-cffi::fade-out-channel channel (if fade fade 0))
		    (sdl-mixer-cffi::expire-channel channel (if ticks ticks 0))))))))

(defun linked-version ()
    "Returns the version number of the SDL_mixer dynamic library in use as #\(`MAJOR` `MINOR` `PATCH`\)."
  (let ((sdl-version (sdl-mixer-cffi::linked-version))
	(version nil))
    (cffi:with-foreign-slots ((sdl-cffi::major sdl-cffi::minor sdl-cffi::patch) sdl-version sdl-cffi::sdl-version)
      (setf version (vector sdl-cffi::major sdl-cffi::minor sdl-cffi::patch)))
    (cffi:foreign-free sdl-version)
    version))

(defun query-spec ()
   (cffi:with-foreign-objects ((frequency :pointer) (format :pointer) (channels :pointer))
     (let ((opened? (sdl-mixer-cffi::query-spec frequency format channels)))
       (if (= 0 opened?)
         opened?
	 (values opened?
		 (cffi:mem-aref frequency :int)
		 (cffi:mem-aref format :unsigned-int)
		 (cffi:mem-aref channels :int))))))

(defun audio-frequency-p ()
  "Returns the current audio frequency of the audio device."
    (multiple-value-bind (opened? frequency format channels)
	(query-spec)
      (declare (ignore opened? format channels))
      frequency))

(defun audio-format-p ()
  "Returns the current audio format of the audio device."
  (multiple-value-bind (opened? frequency format channels)
      (query-spec)
    (declare (ignore opened? frequency channels))
    format))

(defun audio-channels-p ()
  "Returns the number of output channels used by the audio device, e.g. `2` for stereo and `1` for mono.
Does not return the number of mixing channels allocated."
  (multiple-value-bind (opened? frequency format channels)
      (query-spec)
    (declare (ignore opened? frequency format))
    channels))

(defun audio-opened-p ()
  "Returns the number of times the audio device has been opened by [OPEN-AUDIO](#open-audio), or `NIL` if the audio device is closed."
  (multiple-value-bind (opened? frequency format channels)
      (query-spec)
    (declare (ignore frequency format channels))
    opened?))

(defun sample-volume (chunk)
  "Returns the volume of the sample in [CHUNK](#chunk), as an `INTEGER` from 0 to [+MAX-VOLUME+](#+max-volume+)."
  (sdl-mixer-cffi::volume-chunk chunk -1))
(defun (setf sample-volume) (volume chunk)
  "Sets the `VOLUME` of the sample in [CHUNK](#chunk). 
`VOLUME` can be from 0 to [+MAX-VOLUME+](#+max-volume+).
The `VOLUME` will take effect when [CHUNK](#chunk) is mixed into the output.
Returns the previous `VOLUME` for [CHUNK](#chunk)."
  (sdl-mixer-cffi::volume-chunk chunk volume))

(defun channel-volume (channel)
  "Returns the volume of `CHANNEL`, as an `INTEGER`.
Volume can be from 0 to [+MAX-VOLUME+](#+max-volume+).
If `CHANNEL` is NIL then the average volume over all channels is returned."
  (sdl-mixer-cffi::volume channel (if (integerp channel) channel -1)))
(defun (setf channel-volume) (volume channel)
  "Sets the `VOLUME` of `CHANNEL`. 
`VOLUME` can be from 0 to [+MAX-VOLUME+](#+max-volume+).
If `CHANNEL` is NIL then the volume for all channels is set.
Returns the current `VOLUME` for `CHANNEL` as an `INTEGER` from 0 to [+MAX-VOLUME+](#+max-volume+)."
  (sdl-mixer-cffi::volume (if (integerp channel) channel -1) volume))

(defun music-volume ()
  "Returns current music volume as an `INTEGER` from 0 to [+MAX-VOLUME+](#+max-volume+)."
  (sdl-mixer-cffi::volume-music -1))
(defun (setf music-volume) (volume)
  "Sets the music `VOLUME`.
`VOLUME` can be from `0` to [+MAX-VOLUME+](#+max-volume+).
Has no effect when a fade effect is being performed or when using an external music player.
Returns the previous music `VOLUME` as an `INTEGER` from 0 to [+MAX-VOLUME+](#+max-volume+)"
  (sdl-mixer-cffi::volume-music volume))

(defmethod free ((chunk sdl-mixer-cffi::chunk))
  "Frees the sample in [CHUNK](#chunk). Do not reuse a [CHUNK](#chunk) once freed. Do not attempt to free a [CHUNK](#chunk) that is still being played."
  (sdl:free chunk))

(defmethod free ((music sdl-mixer-cffi::music))
  "Frees the music in [MUSIC](#music). Stops [MUSIC](#music) if currently playing. 
Will block until any current fade effect completes. 
 Do not reuse [MUSIC](#music) once freed."
  (sdl:free music))

(defun sample-playing-p (channel)
  "Returns `T` if a sample is currently playing or is paused on `CHANNEL`, or `NIL` if the sample is halted.
Returns the number of samples playing or paused when `CHANNEL` is `T` or `NIL`."
  (let ((playing? (sdl-mixer-cffi::playing (if (integerp channel) channel -1))))
    (if (= 0 playing?)
	nil
	playing?)))
(defun sample-halted-p (channel)
  "Returns `T` if a sample on `CHANNEL` is currently halted, or `NIL` otherwise."
  (let ((playing? (sdl-mixer-cffi::playing channel)))
    (if (= 0 playing?)
	t
	nil)))

(defun music-playing-p ()
  "Returns `T` if music is currently playing, or `NIL` if music is halted
or paused."
  (let ((playing? (sdl-mixer-cffi::playing-music)))
    (if (= 0 playing?)
      nil
      (if (music-paused-p)
        nil
        playing?))))
(defun music-halted-p ()
  "Returns `T` if music is currently halted, or `NIL` otherwise."
  (let ((playing? (sdl-mixer-cffi::playing-music)))
    (if (= 0 playing?)
	t
	nil)))

(defun sample-paused-p (channel)
  "Returns `T` if the sample on `CHANNEL` is currently paused or was previously paused 
prior to a halt, or `NIL` otherwise.
Returns the number of paused samples when `CHANNEL` is `T` or `NIL`."
  (let ((paused? (sdl-mixer-cffi::paused (if (integerp channel) channel -1))))
    (if (= 0 paused?)
	nil
	paused?)))
(defun music-paused-p ()
  "Returns T if music is currently paused or was previously paused prior to a halt, 
or NIL otherwise."
  (let ((paused? (sdl-mixer-cffi::paused-music)))
    (if (= 0 paused?)
	nil
	paused?)))

(defun sample-fading-p (channel)
  "Returns `T` if a fade is in effect for the sample on `CHANNEL`, regardless of the current play or pause state of the sample.
Returns `:FADING-OUT` if a fade-out is in effect,
`:FADING-IN` if a fade-in is in effect, or 
`NIL` if no fade is in effect."
  (let ((fading? (sdl-mixer-cffi::fading-channel channel)))
    (if (= 0 fading?)
	nil
	(cffi:foreign-enum-keyword 'sdl-mixer-cffi::fading fading?))))
(defun music-fading-p ()
    "Returns the current fade effect on music regardless of the current play or pause state.
Returns `:FADING-OUT` if a fade-out is in effect,
`:FADING-IN` if a fade-in is in effect, or `NIL` if no fade is in effect."
  (let ((fading? (sdl-mixer-cffi::fading-music)))
    (if (= 0 fading?)
	nil
	(cffi:foreign-enum-keyword 'sdl-mixer-cffi::fading fading?))))

(defun pause-music ()
  "Pause music. Music can only be paused if it is actively playing. 
You may halt paused music."
  (sdl-mixer-cffi::pause-music))
(defun pause-sample (channel)
  "Pauses the sample on `CHANNEL`, or `T` to pause samples on all channels. 
`NIL` will resume samples on all channels, same as [RESUME-SAMPLE](#resume-sample).
Only samples which are actively playing will be paused. You may halt a paused sample."
  (if channel
      (sdl-mixer-cffi::pause (if (integerp channel) channel -1))
      (sdl-mixer-cffi::resume -1)))

(defun resume-music ()
  "Resume playing music when paused music. Safe to use on halted and already playing music."
  (sdl-mixer-cffi::resume-music))
(defun resume-sample (channel)
  "Resumes playback of the sample on `CHANNEL`, or `T` to resume playback of samples on all channels. 
`NIL` will pause samples on all channels, same as [PAUSE-SAMPLE](#pause-sample).
Only samples which are paused will be resumed."
  (if channel
      (sdl-mixer-cffi::resume (if (integerp channel) channel -1))
      (sdl-mixer-cffi::pause -1)))

(defun rewind-music ()
  "Rewind to the start of music. Safe to use on halted, paused, and currently playing music. 
It is not necessary to rewind the music immediately after starting playback, 
as it starts at the beginning by default.
Only the following streams support rewind: `MOD`, `OGG`, `MP3`, `Native MIDI`."
  (sdl-mixer-cffi::rewind-music))

(defun music-position (position)
  "Sets the play `POSITION` of the currently playing music. 
Returns `T` on success and `NIL` on failure.
Applicable only to `MOD`, `OGG` and `MP3` music formats as described below.

* `MOD`, jumps to the pattern number in the module identified by `POSITION`. `0` will rewind the module to the beginning.
* `OGG`, jumps to `POSITION` seconds from the beginning of the song.
* `MP3`, jumps forwards `POSITION` seconds from the current play position. Negative values are ignored.
To rewind, use [REWIND-MUSIC](#rewind-music) to jump to the start of the song, then jump forward `POSITION` seconds using [MUSIC-POSITION](#music-position)."
  (if (= 0 (sdl-mixer-cffi::set-music-position position))
      t
      nil))

(defun music-type-p (music)
  "Returns the format type of [MUSIC](#music) as one of `WAV`, `MOD`, `MID`, `OGG`, `MP3`, `MP3-MAD`, `FLAC` or `CMD`.
Returns the format type of the currently playing music when [MUSIC](#music) is `NIL`.
Returns `NIL` is no music is playing."
  (let ((ret (sdl-mixer-cffi::get-music-type (if music music (cffi:null-pointer)))))
    (if ret
	(let ((type? (cffi:foreign-enum-keyword 'sdl-mixer-cffi::music-type ret)))
	  (if (eq type? :MUS-NONE)
	      nil
	      type?)))))

(defun music-type-of (music type)
  "Returns `T` if [MUSIC](#music) is of `TYPE`, returns `NIL` otherwise. 
`TYPE` may be one of `WAV`, `MOD`, `MID`, `OGG`, `MP3`, `MP3-MAD`, `FLAC` or `CMD`.
If [MUSIC](#music) is `NIL` returns the type of the currently playing music. Returns `NIL` is no music is playing."
  (if (eq (music-type-p music) type)
      t
      nil))

(defun allocate-channels (num-channels)
  "Allocates `NUM-CHANNELS` to be used for mixing samples. Frees all allocated channnels when `NUM-CHANNELS` is `NIL` or `0`.
A negative value for `NUM-CHANNELS` is  ignored. 
Returns the number of channels currently allocated.
Can be called multiple times even if samples are currently playing.
If the current allocation of channels is greater than `NUM-CHANNELS`, 
then channels greater than `NUM-CHANNELS` will be stopped and these resources freed. 
The callback set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished) is called for each channel halted.
NOTE: Samples will continue playing when `NUM-CHANNELS` is `0`."
  (unless num-channels
    (setf num-channels 0))
  (sdl-mixer-cffi::allocate-channels num-channels))

(defun deallocate-all-channels ()
  "Free all allocated channels. 
Same as calling [ALLOCATE-CHANNELS](#allocate-channels) with `NIL`, or `0`."
  (allocate-channels 0))

(cffi:defcallback channel-finished-proc :void ((channel :int))
  "Called when any channel finishes playback or is halted. `CHANNEL` contains the channel number that has finished."
  (when *channel-finished*
    (funcall *channel-finished* channel))
  ;(cffi:null-pointer)
  )

(defun register-sample-finished-callback (&optional cb)
  (unless cb
    (setf cb (cffi:callback channel-finished-proc)))
  (sdl-mixer-cffi::channel-finished cb))
(defun unregister-sample-finished-callback ()
  (sdl-mixer-cffi::channel-finished (cffi:null-pointer)))

(defun register-sample-finished (func)
  "Sets the callback that is executed when a sample [CHUNK](#chunk) finishes playback or is halted.
`FUNC` is of the format `#\(lambda \(channel\)\)`

##### Example

    \(REGISTER-SAMPLE-FINISHED 
        \(lambda \(channel\)
          \(FORMAT T \"SAMPLE FINISHED ON CHANNEL: ~A\" channel\)\)\)"
  (setf *channel-finished* func))
(defun unregister-sample-finished ()
  "Removes the callback function set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished)."
  (setf *channel-finished* nil))

(cffi:defcallback music-finished-proc :void ()
  "Called when music finishes playback or is halted."
  (when *music-finished*
    (funcall *music-finished*))
  ;;(cffi:null-pointer)
  )

(defun register-music-finished-callback (&optional cb)
  (unless cb
    (setf cb (cffi:callback music-finished-proc)))
  (sdl-mixer-cffi::hook-music-finished cb))
(defun unregister-music-finished-callback ()
  (sdl-mixer-cffi::hook-music-finished (cffi:null-pointer)))

(defun register-music-finished (func)
  "Sets the callback that is executed when [MUSIC](#music) finishes playback or is halted.
`FUNC` is of the format `#\(lambda \(\)\)`

##### Example

    \(REGISTER-MUSIC-FINISHED 
        \(lambda \(\)
          \(FORMAT T \"MUSIC FINISHED\"\)\)\)"
  (setf *music-finished* func))
(defun unregister-music-finished ()
  "Removes the callback function set by set by [REGISTER-MUSIC-FINISHED](#register-music-finished)."
  (setf *music-finished* nil))

(cffi:defcallback fill-audio-buffer :void
    ((user-data :pointer)
     (stream :pointer)
     (len :int))
  (when *audio-buffer*
    (funcall *audio-buffer* user-data stream len))
  ;;(cffi:null-pointer)
  )

(defun register-music-mixer (func)
    "Sets the callback that is executed to fill the music audio output buffer.
`FUNC` is of the format `#\(lambda \(user-data stream len\)\)`

##### Example

    \(REGISTER-MUSIC-MIXER 
        \(lambda \(user stream len\)
          'FILL-THE-AUDIO-OUTPUT-BUFFER\)\)"
    (setf *audio-buffer* func)
    (sdl-mixer-cffi::hook-music (cffi:callback fill-audio-buffer) (cffi:null-pointer)))
(defun unregister-music-mixer ()
    "Removes any callback function set by [REGISTER-MUSIC-MIXER](#register-music-mixer)."
    (setf *audio-buffer* nil)
    (sdl-mixer-cffi::hook-music (cffi:null-pointer) (cffi:null-pointer)))

(defun group-channels (tag &key (channel 0) (end-channel nil))
  "Assigns the group `TAG` to `:CHANNEL`, or the range of channels from `:CHANNEL` to `:END-CHANNEL`.
`TAG` when `NIL` will remove the `TAG` from the channels specified  and reassign these channels to the default group.
Returns the number of channels grouped on success, or `NIL` on failure when an invalid or unallocated `CHANNEL` 
or range of channels is specified.
Use [ALLOCATE-CHANNELS](#allocate-channels) to allocate one or more channels."
  (let ((ret (if end-channel
		 (sdl-mixer-cffi::group-channels channel end-channel (if tag tag -1))
		 (sdl-mixer-cffi::group-channel channel tag))))
    (if (= 0 ret)
	nil
	ret)))

(defun group-channel-p (tag)
  "Returns the number of reserved channels in the group identified by `TAG`. 
Returns the total number of reserved channels in all groups when `NIL`."
  (sdl-mixer-cffi::group-count (if (integerp tag) tag -1)))

(defun group-available (tag)
  "Returns the first available reserved channel in the group identified by `TAG`. 
Returns the first available reserved channel in any group when `T`.
Returns a channel, or `NIL` when no reserved channels in the specified group are available."
  (let ((ret (sdl-mixer-cffi::group-available (if (integerp tag) tag -1))))
    (if (= ret -1)
	nil
	ret)))

(defun group-oldest (tag)
  "Returns the oldest actively playing reserved channel in the group identified by `TAG`. 
When `T` will return the oldest actively playing reserved channel in any group.
Returns a channel, or `NIL` if no channels are actively playing or the group is empty."
  (let ((ret (sdl-mixer-cffi::group-oldest (if (integerp tag) tag -1))))
    (if (= -1 ret)
	nil
	ret)))

(defun group-recent (tag)
  "Returns the most recent actively playing reserved channel in the group identifed by `TAG`.
When 'T' will return the most recent actively playing reserved channel in any group.
Returns a channel, or `NIL` if no channels are actively playing or the group is empty."
  (let ((ret (sdl-mixer-cffi::group-newer (if (integerp tag) tag -1))))
    (if (= -1 ret)
	nil
	ret)))

;; (defun register-panning-effect (channel left right &optional post)
;;   "Registers a panning effect for the `CHANNEL`. The volume for the left and right sound output channels 
;; are specified by `LEFT` and `RIGHT` respectively."

;;   channel 
;; Channel number to register this effect on.
;; Use MIX_CHANNEL_POST to process the postmix stream. 
;; left 
;; Volume for the left channel, range is 0(silence) to 255(loud) 
;; right 
;; Volume for the left channel, range is 0(silence) to 255(loud) 

;; This effect will only work on stereo audio. Meaning you called Mix_OpenAudio with 2 channels (MIX_DEFAULT_CHANNELS). The easiest way to do true panning is to call Mix_SetPanning(channel, left, 254 - left); so that the total volume is correct, if you consider the maximum volume to be 127 per channel for center, or 254 max for left, this works, but about halves the effective volume.
;; This Function registers the effect for you, so don't try to Mix_RegisterEffect it yourself.
;; NOTE: Setting both left and right to 255 will unregister the effect from channel. You cannot unregister it any other way, unless you use Mix_UnregisterAllEffects on the channel.
;; NOTE: Using this function on a mono audio device will not register the effect, nor will it return an error status. 

;; Returns: Zero on errors, such as bad channel, or if Mix_RegisterEffect failed.
;;   (if (= 0 (sdl-mixer-cffi::set-panning (if post
;; 					    lispbuilder-sdl-cffi::+CHANNEL-POST+
;; 					    channel)
;; 					left right))
;;       nil
;;       t))

;; (defun unregister-panning-effect (channel &optional post)
;;   ""
;;   (if (= 0 (sdl-mixer-cffi::set-panning (if post
;; 					    lispbuilder-sdl-cffi::+CHANNEL-POST+
;; 					    channel)
;; 					255 255))))
