
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

(declaim (inline adjust-volume))
(defun adjust-volume (sample volume)
  (sdl:cast-to-int (/ (* sample volume) +max-volume+)))

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

(defun to-s16 (x)
  (- x (* 2 (logand x #x8000))))

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
    :accessor audio-paused-p
    :initform nil)
   (volume
    :accessor audio-volume
    :initform +max-volume+
    :initarg :audio-volume)
   (callback-finished
    :accessor callback-finished
    :initform nil
    :initarg :callback-finished)))

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

(defmethod _play-audio_ ((self audio))
  (if (find self *managed-audio*)
    (resume-audio self)
    (pushnew self *managed-audio*)))

(defmethod _pause-audio_ ((self audio))
  (when (audio-playing-p self)
    (setf (audio-paused-p self) t)))

(defmethod resume-audio ((self audio))
  (when (audio-playing-p self)
    (setf (audio-paused-p self) nil)))

(defmethod halt-sample ((self audio) &optional (ignore-callback nil))
  (setf *managed-audio* (remove self *managed-audio*))
  (unless ignore-callback
    (audio-finished-callback self)))

(defmethod rewind-audio ((self audio) &optional (pos 0))
  (declare (ignore pos))
  (setf (audio-remaining self) (audio-length self)
        (audio-position self) pos))

(defmethod audio-playing-p ((self audio))
  (if (> (audio-remaining self) 0)
    t
    nil))

(defmethod audio-playable-p ((self audio))
  (if (and (> (audio-remaining self) 0)
           (not (audio-paused-p self)))
    self
    nil))

(defmethod print-object ((obj audio) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "buffer: ~A,  length: ~A, remaining: ~A, position: ~A,  frequency: ~A, format: ~A, channels: ~A"
            (audio-buffer obj) (audio-length obj)
            (audio-remaining obj) (audio-position obj)
            (sample-frequency obj) (audio-format obj)
            (output-channels obj))))