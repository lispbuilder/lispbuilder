
(in-package #:sdl)

(defclass mixer ()
  ((requested-sample-frequency
    :accessor requested-sample-frequency)
   (requested-audio-format
    :accessor requested-audio-format)
   (requested-output-channels
    :accessor requested-output-channels)
   (requested-audio-buffer-size
    :accessor requested-audio-buffer-size)
   (obtained-spec
    :reader audio-spec
    :initform (make-instance 'audio-spec))
   (callback
    :initform nil
    :initarg :callback)
   (mixer-opened-p
    :initform nil
    :reader mixer-opened-p)
   (output-audio-buffer-size
    :initform nil
    :reader output-audio-buffer-size)
   (output-buffer
    :initform nil
    :accessor output-buffer)
   (volume
    :accessor audio-volume
    :initarg :audio-volume))
  (:default-initargs
   :audio-format +DEFAULT-FORMAT+
   :sample-frequency +DEFAULT-FREQUENCY+
   ;:audio-mixer-channels +CHANNELS+
   :output-channels +DEFAULT-CHANNELS+
   :audio-buffer-size +DEFAULT-SAMPLE-BUFFER+
   :volume +MAX-VOLUME+
   :user-data (cffi:null-pointer)))

(defmethod initialize-instance :after ((self mixer)
                                       &key
                                       audio-format
                                       sample-frequency
                                       output-channels
                                       audio-buffer-size
                                       user-data
                                       &allow-other-keys)
  ;; Set the desired sample frequency.
  (setf (requested-sample-frequency self) sample-frequency)
  ;; Set the desired audio format.
  (setf (requested-audio-format self) audio-format)
  ;; Set the number of output channels.
  (setf (requested-output-channels self) output-channels)
  ;; Set the output buffer size
  (setf (requested-audio-buffer-size self) audio-buffer-size)  
  ;; Set a default callback function, if not specified.
  (unless (slot-value self 'callback)
    (setf (slot-value self 'callback) (cffi:callback default-fill-audio-buffer)))
  ;; Set the user data pointer.
  (setf (cffi:foreign-slot-value (sdl:fp (audio-spec self))
                                 'sdl-cffi::SDL-Audio-Spec
                                 'sdl-cffi::userdata) user-data))

(defmethod print-object ((obj mixer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "frequency: ~A,  format: ~A, channels: ~A, samples: ~A, size: ~A, opened-p: ~A"
            (sample-frequency obj) (audio-format obj)
            (output-channels obj) (audio-buffer-size obj)
            (output-audio-buffer-size obj) (mixer-opened-p obj))))

(defmethod sample-frequency ((self mixer))
  (if (mixer-opened-p self)
    (sample-frequency (audio-spec self))
    (requested-sample-frequency self)))

(defmethod audio-format ((self mixer))
  (if (mixer-opened-p self)
    (audio-format (audio-spec self))
    (requested-audio-format self)))

(defmethod output-channels ((self mixer))
  (if (mixer-opened-p self)
    (output-channels (audio-spec self))
    (requested-output-channels self)))

(defmethod audio-buffer-size ((self mixer))
  (if (mixer-opened-p self)
    (audio-buffer-size (audio-spec self))
    (requested-audio-buffer-size self)))   

(defun open-audio (&key
                   (frequency +DEFAULT-FREQUENCY+)
                   (format +DEFAULT-FORMAT+)
                   (channels +DEFAULT-CHANNELS+)
                   (audio-buffer-size +DEFAULT-SAMPLE-BUFFER+)
                   callback
                   (volume +MAX-VOLUME+))
  ;; Make sure that SDL is initialized with SDL:SDL-INIT-AUDIO,
  ;; If not, then initialize it and add it to the list of subsystems to quit on
  ;; sdl exit.
  (when (= (sdl:return-subsystems-of-status SDL:SDL-INIT-AUDIO t) 0)
    (sdl:initialize-subsystems-on-startup (logior
                                           sdl::*initialize-subsystems-on-startup*
                                           SDL:SDL-INIT-AUDIO))
    (sdl:quit-subsystems-on-exit (logior
                                  sdl::*quit-subsystems-on-exit*
                                  SDL:SDL-INIT-AUDIO))
    ;; Initialize the subsystems in sdl::*initialize-subsystems-on-startup*
    ;; that are not yet initialized.
    (sdl:init-subsystems))
  (setf *mixer* (make-instance 'mixer
                               :audio-volume volume
                               :audio-format format
                               :sample-frequency frequency
                               :output-channels channels
                               :audio-buffer-size audio-buffer-size
                               :callback callback))

  (cffi:with-foreign-object (requested-audio-spec 'sdl-cffi::SDL-Audio-Spec)
    ;; Set the desired sample frequency.
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::SDL-Audio-Spec
                                   'sdl-cffi::freq)
          (requested-sample-frequency *mixer*))
    ;; Set the desired audio format.
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::SDL-Audio-Spec
                                   'sdl-cffi::format)
          (requested-audio-format *mixer*))
    ;; Set the number of output channels.
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::SDL-Audio-Spec
                                   'sdl-cffi::channels)
          (requested-output-channels *mixer*))
    ;; Set the output buffer size
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::sdl-Audio-Spec
                                   'sdl-cffi::samples)
          (requested-audio-buffer-size *mixer*))
    ;; Set the callback function, if not specified
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::SDL-Audio-Spec
                                   'sdl-cffi::callback)
          (slot-value *mixer* 'callback))
  
    (if (eql -1 (sdl-cffi::sdl-open-audio requested-audio-spec
                                          (sdl:fp (audio-spec *mixer*))))
      (setf (slot-value *mixer* 'mixer-opened-p) nil)
      (setf (slot-value *mixer* 'mixer-opened-p) t))
    (when (mixer-opened-p *mixer*)
      (setf (slot-value *mixer* 'output-audio-buffer-size)
            (if (or (equal (audio-format *mixer*) sdl-cffi::audio-u16)
                    (equal (audio-format *mixer*) sdl-cffi::audio-s16))
              (* 2 (audio-buffer-size *mixer*))
              (audio-buffer-size *mixer*)))
      *mixer*)))

(defun audio-opened-p ()
  (when *mixer*
    (slot-value *mixer* 'mixer-opened-p)))

(defun play-audio (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_play-audio_ obj))

(defmethod _play-audio_ ((self mixer))
  (sdl-cffi::sdl-pause-audio 0))

(defun pause-audio (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_pause-audio_ obj))

(defmethod _pause-audio_ ((self mixer))
  (sdl-cffi::sdl-pause-audio 1))

(defun close-audio ()
  (when (audio-opened-p)
    ;; Pause the audio stream
    (pause-audio)
    ;; Lock the audio device to halt all callbacks
    (sdl-cffi::sdl-lock-audio)
    ;; Close the audio device
    (sdl-cffi::sdl-close-audio)
    (setf (slot-value *mixer* 'mixer-opened-p) nil)
    (setf *managed-audio* nil)))

(defun load-sample (filename)
  (let ((spec (make-instance 'audio-spec))
        (sample-handle (cffi:foreign-alloc :pointer)))
    (cffi:with-foreign-object (sample-length :pointer)
      (unless (cffi:null-pointer-p (sdl-cffi::sdl-load-wav filename
                                                           (audio-spec spec)
                                                           sample-handle
                                                           sample-length))
        ;; Determine the format of the audio buffer
        ;; We are going to save the buffer in this format to save on storage,
        ;; and reduce bit-fiddling later on.
        (let* ((wave (cffi:mem-aref sample-handle :pointer))
               (buff (cond
                      ((or (= (audio-format spec) sdl-cffi::AUDIO-U8)
                           (= (audio-format spec) sdl-cffi::AUDIO-S8))
                       (loop repeat (cffi:mem-aref sample-length :unsigned-int)
                             for byte = 0 then (1+ byte)
                             collect (cffi:mem-aref wave :unsigned-char byte)))
                      ((or (= (audio-format spec) sdl-cffi::AUDIO-U16LSB)
                           (= (audio-format spec) sdl-cffi::AUDIO-S16LSB))
                       (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                             for byte-1 = 0 then (+ 2 byte-1)
                             for byte-2 = 1 then (1+ byte-1)
                             collect (logior (ash (cffi:mem-aref wave :unsigned-char byte-2) 8)
                                             (cffi:mem-aref wave :unsigned-char byte-1))))
                      ((or (= (audio-format spec) sdl-cffi::AUDIO-U16MSB)
                           (= (audio-format spec) sdl-cffi::AUDIO-S16MSB))
                       (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                             for byte-1 = 0 then (+ 2 byte-1)
                             for byte-2 = 1 then (1+ byte-1)
                             collect (logior (ash (cffi:mem-aref wave :unsigned-char byte-1) 8)
                                             (cffi:mem-aref wave :unsigned-char byte-2)))))))
          (sdl-cffi::sdl-free-wav wave)
          (make-instance 'audio
                         :audio-buffer-handle
                         (make-instance 'audio-buffer
                                        :audio-buffer (make-array
                                                       (length buff)
                                                       :element-type (from-sdl-type
                                                                      (audio-format spec))
                                                       :initial-contents buff)
                                        :audio-length (length buff)
                                        :audio-spec spec)))))))

(defmethod build-audio-cvt ((audio audio) (mixer mixer))
  (cffi:with-foreign-object (audio-cvt 'sdl-cffi::sdl-audio-cvt)
    (when (> (sdl-cffi::sdl-build-audio-cvt audio-cvt
                                            (audio-format audio)
                                            (output-channels audio)
                                            (sample-frequency audio)
                                            (audio-format mixer)
                                            (output-channels mixer)
                                            (sample-frequency mixer))
             0)
      (cffi:with-foreign-slots ((sdl-cffi::buf sdl-cffi::len sdl-cffi::len-mult
                                               sdl-cffi::len-ratio
                                               sdl-cffi::len-cvt)
                                audio-cvt sdl-cffi::sdl-audio-cvt)
        (setf sdl-cffi::len (audio-length audio)
              sdl-cffi::buf (cffi:foreign-alloc :unsigned-char
                                                :count (* (audio-length audio)
                                                          sdl-cffi::len-mult))
              sdl-cffi::len-cvt (* (audio-length audio) sdl-cffi::len-mult))

        ;; Need to fix this to convert from source format to destination format.
        ;; Assumes source is uint8 right now.
        (dotimes (i (audio-length audio))
          (setf (cffi:mem-aref sdl-cffi::buf :unsigned-char i)
                    (aref (audio-buffer audio) i)))
        
        (when (> (sdl-cffi::sdl-convert-audio audio-cvt) -1)
          (let* ((buff (cond
                        ;; U16LSB or S16LSB
                        ((lsb-p (audio-format mixer))
                         (loop repeat (calculated-buffer-length
                                       sdl-cffi::len-cvt
                                       sdl-cffi::AUDIO-U8
                                       (audio-format mixer))
                               for byte-1 = 0 then (+ 2 byte-1)
                               for byte-2 = 1 then (1+ byte-1)
                               collect (to-s16 (logior (ash (cffi:mem-aref sdl-cffi::buf
                                                                   :unsigned-char byte-2) 8)
                                               (cffi:mem-aref sdl-cffi::buf
                                                              :unsigned-char byte-1)))))
                        ;; U16MSB or S16MSB
                        ((msb-p (audio-format mixer))
                         (loop repeat (calculated-buffer-length sdl-cffi::len-cvt
                                                                sdl-cffi::AUDIO-U8
                                                                (audio-format mixer))
                               for byte-1 = 0 then (+ 2 byte-1)
                               for byte-2 = 1 then (1+ byte-1)
                               collect (logior (ash (cffi:mem-aref sdl-cffi::buf
                                                                   :unsigned-char byte-1) 8)
                                               (cffi:mem-aref sdl-cffi::buf
                                                              :unsigned-char byte-2))))
                        ;; Assume u8 or s8
                        (t
                         (loop repeat (calculated-buffer-length
                                       sdl-cffi::len-cvt
                                       sdl-cffi::AUDIO-U8
                                       (audio-format mixer))
                               for byte = 0 then (1+ byte)
                               collect (cffi:mem-aref sdl-cffi::buf
                                                      :unsigned-char byte))))))
            (sdl-cffi::sdl-free-wav sdl-cffi::buf)
            (make-instance 'audio
                           :audio-buffer-handle
                           (make-instance 'audio-buffer
                                          :audio-buffer
                                          (make-array (length buff)
                                                      :element-type (from-sdl-type
                                                                     (audio-format mixer))
                                                      :initial-contents buff)
                                          :audio-length
                                          (calculated-buffer-length
                                           (sdl:cast-to-int
                                            (* sdl-cffi::len
                                               sdl-cffi::len-ratio))
                                           sdl-cffi::AUDIO-U8
                                           (audio-format mixer))
                                          :frequency
                                          (sample-frequency mixer)
                                          :format
                                          (audio-format mixer)
                                          :channels
                                          (output-channels mixer)))))))))

(defun load-audio (filename)
  (let ((audio-buffer (load-sample filename)))
    (when (and audio-buffer (audio-opened-p))
      (build-audio-cvt audio-buffer *mixer*))))


(cffi:defcallback default-fill-audio-buffer
    :pointer ((user-data :pointer)
              (stream :pointer)
              (len :int))
  (declare (ignore user-data))
  ;; The callback is asking for an amount of 'len' uint8 bytes.
  ;; Have to convert this to the mixer format, as this is what all the audio
  ;; buffers will be in.
  (let ((len (calculated-buffer-length len sdl-cffi::audio-u8
                                       (audio-format *mixer*))))
    ;; If the callback function is asking for more data than the existing output
    ;; buffer, then create a new output buffer of the requested length.
    (if (/= len (length (output-buffer *mixer*)))
      (setf (output-buffer *mixer*) (make-array len
                                                :initial-element
                                                (audio-silence (audio-spec *mixer*))
                                                :element-type 'fixnum))
      ;; Write silence to the output buffer
      (overwrite-all (output-buffer *mixer*) (audio-silence (audio-spec *mixer*))))
    (let ((output (output-buffer *mixer*))
          (master-volume (audio-volume *mixer*)))
      (dolist (audio *managed-audio*)
        (when (audio-playable-p audio)
          ;; Play audio only when audio remains in the sample.
          (when (> (audio-remaining audio) 0)
            (let* ((len (if (> len (audio-remaining audio))
                          (audio-remaining audio)
                          len))
                   (buffer (audio-buffer audio))
                   (volume (audio-volume audio)))
              (loop for i from 0 below len
                    for j = (audio-position audio) then (1+ j)
                    do (incf (aref output i)
                             (adjust-volume (aref buffer j) volume)))
              (incf (audio-position audio) len)
              (decf (audio-remaining audio) len)))))
      ;; Sample finished playing, then remove.
      
      ;; Set mixer volume limits.
      (let ((max-audioval (if (= 2 (element-size (audio-format *mixer*)))
                            +max-audio-16+
                            +max-audio-8+))
            (min-audioval (if (= 2 (element-size (audio-format *mixer*)))
                            +min-audio-16+
                            +min-audio-8+)))
        (loop for val across output
              for x = 0 then (+ x 2)
              for y = 1 then (+ x 1)
              for adjusted = (adjust-volume val master-volume)
              for clamped = (cond
                             ((> adjusted max-audioval)
                              max-audioval)
                             ((< adjusted min-audioval)
                              min-audioval)
                             (t adjusted)) do
              (setf (mem-aref stream :unsigned-char x) (logand clamped #xFF)
                    (mem-aref stream :unsigned-char y) (logand (ash clamped -8) #xFF))))))
    (cffi:null-pointer))
