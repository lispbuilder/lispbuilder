
(in-package #:sdl)

(defun wav-free (func-fp type)
  (declare (ignorable type))
  #'(lambda (obj-fp)
      (when (sdl:is-valid-ptr obj-fp)
        ;; Free the audio wav-buffer
        ;; NOTE: Dereference the HANDLE returned from SDL-LOAD-WAV and pass
        ;; this to SDL-FREE-WAV or a memory leak *will* result.
        (funcall func-fp
                 (cffi:make-pointer (cffi:pointer-address
                                     (cffi:mem-aref obj-fp :pointer)))))))

;(declaim (inline adjust-volume))
(defun adjust-volume (sample volume)
  (sdl:cast-to-int (/ (* sample volume) +max-volume+)))

;(declaim (inline adjust-volume-8))
(defun adjust-volume-u8 (sample volume)
  (sdl:cast-to-int (+ (/ (* (- sample 128) volume) +max-volume+) 128)))

(defun from-sdl-type (format)
  (cond
   ((= format sdl-cffi::AUDIO-U8)
    '(unsigned-byte 8))
   ((= format sdl-cffi::AUDIO-S8)
    '(signed-byte 8))
   ((or (= format sdl-cffi::AUDIO-U16LSB)
        (= format sdl-cffi::AUDIO-U16MSB))
    '(unsigned-byte 16))
   ((or (= format sdl-cffi::AUDIO-S16LSB)
        (= format sdl-cffi::AUDIO-S16MSB))
    '(signed-byte 16))))

(defun overwrite-all (array value &optional (start 0) (end nil))
  (loop for i from start below (if end end (length array)) do
        (setf (aref array i) value))
  array)

(defun element-size (format)
  (cond
   ((or (= format sdl-cffi::AUDIO-U8)
        (= format sdl-cffi::AUDIO-S8))
    1)
   ((or (= format sdl-cffi::AUDIO-U16LSB)
        (= format sdl-cffi::AUDIO-S16LSB)
        (= format sdl-cffi::AUDIO-U16MSB)
        (= format sdl-cffi::AUDIO-S16MSB))
    2)))

(defun calculated-buffer-length (len src-format dst-format)
  (cond
   ((= (element-size src-format)
       (element-size dst-format))
    len)
   ((> (element-size src-format)
       (element-size dst-format))
    (* len 2))
   ((< (element-size src-format)
       (element-size dst-format))
    (/ len 2))))

(defun lsb-p (format)
  (when (or (= format sdl-cffi::AUDIO-U16LSB)
            (= format sdl-cffi::AUDIO-S16LSB))
    t))

(defun msb-p (format)
  (when (or (= format sdl-cffi::AUDIO-U16MSB)
            (= format sdl-cffi::AUDIO-S16MSB))
    t))

;(declaim (inline to-s16))
(defun to-s16 (x)
  (- x (* 2 (logand x #x8000))))

;(declaim (inline to-s8))
(defun to-s8 (x)
  (- x (* 2 (logand x #x80))))

;(declaim (inline from-s16))
(defun from-s16 (x)
  (if (< x 0)
    (+ x 65536)
    x))

;(declaim (inline from-s8))
(defun from-s8 (x)
  (if (< x 0)
    (+ x 256)
    x))

(defclass audio-spec (sdl::foreign-object)()
  (:default-initargs
   :fp (cffi:foreign-alloc 'sdl-cffi::SDL-Audio-Spec)
   :gc t
   :free #'cffi:foreign-free))

(defmethod print-object ((obj audio-spec) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "frequency: ~A,  format: ~A, channels: ~A, silence: ~A,  samples: ~A, size: ~A"
            (sample-frequency obj) (audio-format obj)
            (output-channels obj) (audio-silence obj)
            (audio-buffer-size obj)
            (audio-buffer-size-calculated obj))))

(defmethod audio-spec ((self audio-spec))
  (sdl:fp self))

(defmethod sample-frequency ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::freq))

(defmethod audio-format ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::format))

(defmethod output-channels ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::channels))

(defmethod audio-silence ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::silence))

(defmethod audio-buffer-size ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::samples))

(defmethod audio-buffer-size-calculated ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::size))

(defmethod spec-callback ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::callback))

(defmethod spec-user-data ((self audio-spec))
  (cffi:foreign-slot-value (audio-spec self)
                           'sdl-cffi::sdl-Audio-Spec
                           'sdl-cffi::userdata))

(defclass audio-cvt (sdl::foreign-object) ()
  (:default-initargs
   :fp (cffi:foreign-alloc 'sdl-cffi::sdl-audio-cvt)
   :gc t
   :free #'cffi:foreign-free))

(defclass audio-buffer ()
  ((buffer
    :reader audio-buffer
    :initform nil
    :initarg :audio-buffer)
   (length
    :reader audio-length
    :initform 0
    :initarg :audio-length)
   (spec
    :reader audio-spec
    :initform nil
    :initarg :audio-spec)
   (frequency
    :initarg :frequency)
   (format
    :initarg :format)
   (channels
    :initarg :channels)
   (converted-p
    :reader converted-p
    :initform nil)))

(defmethod sample-frequency ((self audio-buffer))
  (if (audio-spec self)
    (sample-frequency (audio-spec self))
    (slot-value self 'frequency)))

(defmethod audio-format ((self audio-buffer))
  (if (audio-spec self)
    (audio-format (audio-spec self))
    (slot-value self 'format)))

(defmethod output-channels ((self audio-buffer))
  (if (audio-spec self)
    (output-channels (audio-spec self))
    (slot-value self 'channels)))

(defclass audio ()
  ((buffer-handle
    :reader audio-buffer-handle
    :initform nil
    :initarg :audio-buffer-handle)
   (remaining
    :accessor audio-remaining
    :initform 0)
   (position
    :accessor audio-position
    :initform 0)
   (pause
    :initform nil)
   (volume
    :accessor audio-volume
    :initform +max-volume+
    :initarg :audio-volume)
   (play-count
    :accessor play-count
    :initform 1
    :initarg :play-count)
   (loop
    :accessor loop-p
    :initform nil
    :initarg :loop)
   (callback-finished
    :accessor callback-finished
    :initform nil
    :initarg :callback-finished)
   (remove-p
    :accessor remove-audio-p
    :initform nil)))

(defmethod initialize-instance :after ((self audio)
                                       &key start &allow-other-keys)
  (when start
    (setf (audio-position self) start))
  (when (audio-buffer-handle self)
    (setf (audio-remaining self) (audio-length self))))

(defmethod copy-audio ((self audio))
  (make-instance 'audio
                 :audio-buffer-handle (audio-buffer-handle self)
                 :callback-finished (callback-finished self)))

(defmethod audio-buffer ((self audio))
  (audio-buffer (audio-buffer-handle self)))

(defmethod audio-length ((self audio))
  (audio-length (audio-buffer-handle self)))

(defmethod sample-frequency ((self audio))
  (sample-frequency (audio-buffer-handle self)))

(defmethod audio-format ((self audio))
  (audio-format (audio-buffer-handle self)))

(defmethod output-channels ((self audio))
  (output-channels (audio-buffer-handle self)))

(defmethod register-audio-finished ((self audio) fn)
  (when fn
    (setf (callback-finished self) fn)))

(defmethod unregister-audio-finished ((self audio))
  (setf (callback-finished self) nil))

(defmethod audio-finished-callback ((self audio))
  (funcall (callback-finished self)))

(defmethod _play-audio_ ((self audio) &key loop pos)
  "Plays `AUDIO` from start if not already playing or halted.
Rewinds and plays `AUDIO` if paused or halted."
  (declare (ignore pos))
  (unless (find self *managed-audio*)
    (pushnew self *managed-audio*))
  (setf (slot-value self 'loop) loop)
  (when (numberp (slot-value self 'loop))
    (setf (play-count self) loop))
  (rewind-audio self)
  (resume-audio self))

(defmethod rewind-audio ((self audio) &optional (pos 0))
  "Rewind to start of `AUDIO`. Safe to use on halted, paused or currently
playing music. Does not resume or begin playback of halted or paused music."
  (setf (audio-remaining self) (audio-length self)
        (audio-position self) pos))

(defmethod _pause-audio_ ((self audio))
  "Pauses playback of `AUDIO`. Only `AUDIO` that is actively playing will be paused.
You may halt a paused sample."
  (when (find self *managed-audio*)
    (setf (slot-value self 'pause) t)))

(defmethod _resume-audio_ ((self audio))
  "Resumes playback of `AUDIO`. Only paused `AUDIO` will be resumed."
  (when (find self *managed-audio*)
    (setf (slot-value self 'pause) nil)))

(defmethod halt-audio ((self audio) &optional (ignore-callback nil))
  "Stops playback of `AUDIO`."
  (when (find self *managed-audio*)
    (setf (remove-audio-p self) t))
  (unless ignore-callback
    (audio-finished-callback self)))

(defmethod audio-halted-p ((self audio))
  "Returns `T` if `AUDIO` is currently halted, or `NIL` otherwise."
  (not (find self *managed-audio*)))

(defmethod _audio-paused-p_ ((self audio))
  (when (find self *managed-audio*)
    (slot-value self 'pause)))

(defmethod _audio-playing-p_ ((self audio))
  "Returns `T` if `AUDIO` is currently playing or is paused,
or `NIL` if `AUDIO` is halted."
  (when (or (not (audio-halted-p self))
            (audio-paused-p self))
    t))

(defmethod post-process ((self audio))
  ;; Determine if sample is complete.
  (let ((complete nil))
    (if (loop-p self)
      (when (numberp (play-count self))
        (decf (play-count self))
        (if (<= (play-count self) 0)
          (setf complete t)))
      (setf complete t))
    (if complete
      (setf (remove-audio-p self) self)
      (rewind-audio self))
    complete))

(defun fill-output-buffer (output audio &key len)
  (unless len
    (setf len (length output)))
  ;; Fill the output buffer with data from the audio buffer.
  (let ((quit nil))
    (loop until quit
          until (<= len 0) do
          (progn
            (if (> (audio-remaining audio) 0)
              (let* ((length (if (> len (audio-remaining audio))
                               (audio-remaining audio)
                               len))
                     (buffer (audio-buffer audio))
                     (volume (audio-volume audio)))
                (loop for i from 0 below length
                      for j = (audio-position audio) then (1+ j)
                      do (incf (aref output i)
                               (adjust-volume (aref buffer j) volume)))
                (incf (audio-position audio) length)
                (decf (audio-remaining audio) length)
                (decf len length))
              (setf quit (post-process audio)))))))

(defmethod print-object ((obj audio) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "buffer: ~A,  length: ~A, remaining: ~A, position: ~A,  frequency: ~A, format: ~A, channels: ~A"
            (audio-buffer obj) (audio-length obj)
            (audio-remaining obj) (audio-position obj)
            (sample-frequency obj) (audio-format obj)
            (output-channels obj))))