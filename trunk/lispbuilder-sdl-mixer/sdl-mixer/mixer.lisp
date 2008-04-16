
(in-package #:sdl-mixer)


;; Configure Lispworks to allow
;; callbacks from foreign threads
#+lispworks(system:setup-for-alien-threads)

;;;; Add documentation strings for defconstants in cffi/sdl-mixer.lisp
(setf (documentation 'sdl-mixer:+default-format+ 'variable)
      "Default SDL audio format; little-endian is `AUDIO-S16LSB`, big-endian is `AUDIO-S16MSB`.
Audio formats are defined in `SDL_audio.h`;

* `AUDIO_U8` : Unsigned 8-bit samples
* `AUDIO_S8` : Signed 8-bit samples
* `AUDIO_U16LSB` : Unsigned 16-bit samples, in little-endian byte order
* `AUDIO_S16LSB` : Signed 16-bit samples, in little-endian byte order
* `AUDIO_U16MSB` : Unsigned 16-bit samples, in big-endian byte order
* `AUDIO_S16MSB` : Signed 16-bit samples, in big-endian byte order
* `AUDIO_U16` : same as `AUDIO_U16LSB` (for backwards compatability probably)
* `AUDIO_S16` : same as `AUDIO_S16LSB` (for backwards compatability probably)
* `AUDIO_U16SYS` : Unsigned 16-bit samples, in system byte order
* `AUDIO_S16SYS` : Signed 16-bit samples, in system byte order")


(setf (documentation 'sdl-mixer:sdl-mixer-version 'function)
      "Sets the `SDL-VERSION` structure with the version of the library..")
(setf (documentation 'sdl-mixer:+channels+ 'variable)
      "Default number of `8` mixer channels.")
(setf (documentation 'sdl-mixer:+default-frequency+ 'variable)
      "Default sampling frequency of `22,050hz`")
(setf (documentation 'sdl-mixer:+default-channels+ 'variable)
      "Default number of `2` sound channels for Stereo output.")
(setf (documentation 'sdl-mixer:+max-volume+ 'variable)
      "Default volume of `128` for samples, output channels and mix channels.")
(setf (documentation 'sdl-mixer:+channel-post+ 'variable)
      "Default channel of `-2` used for post-processing.")

(defconstant +default-sample-buffer+ 4096
  "Default size of the sample output buffer is 4906 bytes")

(defun load-music (filepath)
  "Loads the music file at location `FILEPATH`. Must be a `WAVE`, `MOD`, `MIDI`, `OGG` or `MP3` file.
Returns music as a new [MUSIC](#music) object, or NIL on error."
  (let ((file (namestring filepath)))
    (if (and (stringp file) (probe-file file))
	(let ((music-fp (sdl-mixer-cffi::LOAD-MUS file)))
	  (if (sdl-base:is-valid-ptr music-fp)
	      (make-instance 'sdl-mixer-cffi::music :fp music-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))

(defun load-sample (filepath)
    "Loads the sample file at location `FILEPATH`. Must be a `WAVE`, `AIFF`, `RIFF`, `OGG`, or `VOC` file.
Returns the sample as a new [CHUNK](#chunk) object, or NIL on error."
  (let ((file (namestring filepath)))
    (if (and (stringp file) (probe-file file))
	(let ((chunk-fp (sdl-mixer-cffi::LOAD-WAV file)))
	  (if (sdl-base:is-valid-ptr chunk-fp)
	      (make-instance 'sdl-mixer-cffi::chunk :fp chunk-fp)
	      (error "Cannot load ~A." file)))
	(error "Music file ~A does not exist." file))))

(defun sample-from-channel (channel)
      "Returns currently playing or most recently played sample on `CHANNEL` as a new [CHUNK](#chunk) object, 
or NIL if `CHANNEL` is not allocated or `CHANNEL` has not yet played out any samples.
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
  "Initializes the mixer.

##### Parameters

* `FREQUENCY` is the output sampling frequency in samples per second (Hz). Default is [+DEFAULT-FREQUENCY+](#+default-frequency+).
* `FORMAT` is the output sample format. Default is [+DEFAULT-FORMAT+](#+default-format+).
* `CHANNELS` sets the number of sound channels in the output. Set to 2 for stereo, 1 for mono. Default is [+DEFAULT-CHANNELS+](#+default-channels+). 
This is not the same Default is [ALLOCATE-CHANNELS](#allocate-channels) which sets the number if mixing channels.
* `CHUNKSIZE` is the bytes used per output sample. Default is [+DEFAULT-SAMPLE-BUFFER+](#+default-sample-buffer+) bytes.

##### Returns

* T on success and NIL on failure.

SDL must be initialized with [SDL-INIT-AUDIO](#sdl-init-audio) prior to this call. 
Most games use a `FREQUENCY` of 22050 [+DEFAULT-FREQUENCY+](#+default-frequency+). A value of 44100 is 44.1KHz is the CD audio rate.
`CHUNKSIZE` is the size of each mixed sample. The smaller the `CHUNKSIZE`, the more frequenctly the mixer hooks are called. 
Increase `CHUNKSIZE` if sound skips or if plaing music. Decrease `CHUNKSIZE` to decrease sound lag.
[+MIX-CHANNELS+](#+channels+) (8) mixing channels are allocated by default. This can be changed by calling [ALLOCATE-CHANNELS](#allocate-channels).
[OPEN-AUDIO](#open-audio) can be called multiple times, however `FORMAT` will not changed on subsequent calls. The device must be closed and re-opened for any change to `FORMAT`.
[CLOSE-AUDIO](#close-audio) must be called the same number of time to close the audio device. "
  ;; Make sure that SDL is initialized with SDL:SDL-INIT-AUDIO
  (when (= 0 (sdl:return-sub-systems-of-status SDL:SDL-INIT-AUDIO t))
    (error "OPEN-AUDIO: SDL must be initialized with SDL:SDL-INIT-AUDIO prior to calling OPEN-AUDIO."))
  (if (= 0 (sdl-mixer-cffi::open-audio frequency format channels chunksize))
      t
      nil))

(defun close-audio (&optional (all nil))
  "Attempts to close the audio device. The audio device can be opened multiple times by [OPEN-AUDIO](#open-audio) 
and to properly close the audio device, [CLOSE-AUDIO](#close-audio) should be called the same number of times. 
Optionally `ALL` when `T` will forcibly close the audio device, no matter how many times the device was opened."
  (if (and all (audio-opened-p))
      (dotimes (i (audio-opened-p))
	(sdl-mixer-cffi::close-audio))
      (sdl-mixer-cffi::close-audio)))

(defun play-music (music &key
		   (loop nil)
		   (fade nil)
		   (position 0))
  "Starts playing `MUSIC` from `POSITION`. The music can be faded-in over the number of milliseconds in `FADE`.
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
		    (channel nil)
		    (loop nil)
		    (fade nil)
		    (ticks t))
  "Plays the sample in `CHUNK` for `TICKS` milliseconds, repeated `LOOP` times.
The sample is repeated the number of times specified in `LOOP`, or until `TICKS`. 
The sample will fade-in over `FADE` milliseconds. 
The callback set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished) is called when the sample stops playing.

##### Parameters

* `CHUNK` is the sample [CHUNK](#chunk).
* `CHANNEL` specifies the channel to play the sample. When `NIL`, will play on the first free unreserved channel. Default is `NIL`.
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

(defun halt-sample (channel &key
		    (fade nil)
		    (ticks nil))
  "Stops playing the sample on `CHANNEL`. 
The callback set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished) is called when the channel stops.

##### Parameters

* `CHANNEL` specifies the channel to stop playing. When `T`, will stop playback samples on all channels. Ignores channels not currently playing out samples.
* `FADE` is the number of milliseconds to perform the fade-out effect. The sample is stopped when fade effect completes.
When `FADE` is `NIL` or `0` the sample is stopped immediately. Default is `NIL`.
* `TICKS` is the number of milliseconds until the sample is stopped. When `NIL` or `0`, the sample is stopped immediately. Default is `NIL`.

##### Returns

* Returns the number of samples fading out, when `FADE` is set.
* Returns the number of samples halted, when `TICKS` is set.
* Otherwise, returns T on success and NIL on failure."
  (if (or (eq channel t) (null channel))
      (setf channel -1))
  (if (and (null fade) (null ticks))
      (progn
	(sdl-mixer-cffi::halt-channel channel)
	t)
      (if (integerp fade)
	  (sdl-mixer-cffi::fade-out-channel channel (if fade fade 0))
	  (sdl-mixer-cffi::expire-channel channel (if ticks ticks 0)))))

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
       (unless (= 0 opened?)
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
  "Returns the volume of the sample in `CHUNK`, as an `INTEGER` from 0 to [+MAX-VOLUME+](#+max-volume+)."
  (sdl-mixer-cffi::volume-chunk chunk -1))
(defun (setf sample-volume) (volume chunk)
  "Sets the `VOLUME` of the sample in `CHUNK`. 
`VOLUME` can be from 0 to [+MAX-VOLUME+](#+max-volume+).
The `VOLUME` will take effect when `CHUNK` is mixed into the output.
Returns the previous `VOLUME` for `CHUNK`."
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
  "Frees the sample in `CHUNK`. Do not reuse `CHUNK` once freed. Do not attempt to free `CHUNK` that is still being played."
  (sdl-mixer-cffi::free chunk))

(defmethod free ((music sdl-mixer-cffi::music))
  "Frees the music in `MUSIC`. Stops `MUSIC` if currently playing. 
Will block until any current fade effect completes. 
 Do not reuse `MUSIC` once freed."
  (sdl-mixer-cffi::free music))

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
  "Returns `T` if music is currently playing or is paused, or `NIL` if music is halted."
  (let ((playing? (sdl-mixer-cffi::playing-music)))
    (if (= 0 playing?)
	nil
	playing?)))
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
  "Returns the format type of `MUSIC` as one of `WAV`, `MOD`, `MID`, `OGG`, `MP3`, `MP3-MAD`, `FLAC` or `CMD`.
Returns the format type of the currently playing music when `MUSIC` is `NIL`.
Returns `NIL` is no music is playing."
  (let ((ret (sdl-mixer-cffi::get-music-type (if music music (cffi:null-pointer)))))
    (if ret
	(let ((type? (cffi:foreign-enum-keyword 'sdl-mixer-cffi::music-type ret)))
	  (if (eq type? :MUS-NONE)
	      nil
	      type?)))))

(defun music-type-of (music type)
  "Returns `T` if `MUSIC` is of `TYPE`, returns `NIL` otherwise. 
`TYPE` may be one of `WAV`, `MOD`, `MID`, `OGG`, `MP3`, `MP3-MAD`, `FLAC` or `CMD`.
If `MUSIC` is `NIL` returns the type of the currently playing music. Returns `NIL` is no music is playing."
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

(cffi:defcallback channel-finished-proc :pointer
    ((channel :int))
  "Called when any channel finishes playback or is halted. `CHANNEL` contains the channel number that has finished."
  (when *channel-finished*
    (funcall *channel-finished* channel))
  (cffi:null-pointer))

(defun register-sample-finished (func)
  "Sets the function that is called when a sample finishes playback or is halted.
`FUNC` is of the format #\(lambda \(channel\)\)."
  (setf *channel-finished* func)
  (sdl-mixer-cffi::channel-finished (cffi:callback channel-finished-proc)))
(defun unregister-sample-finished ()
  "Removes the callback function set by [REGISTER-SAMPLE-FINISHED](#register-sample-finished)."
  (setf *channel-finished* nil)
  (sdl-mixer-cffi::channel-finished (cffi:null-pointer)))


(cffi:defcallback music-finished-proc :pointer
    ()
  "Called when music finishes playback or is halted."
  (when *music-finished*
    (funcall *music-finished*))
  (cffi:null-pointer))

(defun register-music-finished (func)
  "Sets the function that is called when music finishes playback or is halted.
`FUNC` is of the format #\(lambda \(\)\)."
  (setf *music-finished* func)
  (sdl-mixer-cffi::hook-music-finished (cffi:callback music-finished-proc)))
(defun unregister-music-finished ()
  "Removes the callback function set by set by [REGISTER-MUSIC-FINISHED](#register-music-finished)."
  (setf *music-finished* nil)
  (sdl-mixer-cffi::hook-music-finished (cffi:null-pointer)))


(cffi:defcallback fill-audio-buffer :pointer ((user-data :pointer)
					      (stream :pointer)
					      (len :int))
  (when *audio-buffer*
    (funcall *audio-buffer* user-data stream len))
  (cffi:null-pointer))

(defun register-music-mixer (func)
    "Takes a function of the format #\(lambda \(user-data stream len\)\) that is called 
to fill the music audio buffer."
    (setf *audio-buffer* func)
    (sdl-mixer-cffi::hook-music (cffi:callback fill-audio-buffer) (cffi:null-pointer)))
(defun unregister-music-mixer ()
    "Removes any callback function that whould have been called to fill the music audio buffer."
    (setf *audio-buffer* nil)
    (sdl-mixer-cffi::hook-music (cffi:null-pointer) (cffi:null-pointer)))
