
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
    :initarg :audio-volume)
   (loop
    :accessor loop-p
    :initform nil
    :initarg :loop)
   (pause
    :initform nil))
  (:default-initargs
   :audio-format +DEFAULT-FORMAT+
   :sample-frequency +DEFAULT-FREQUENCY+
   ;:audio-mixer-channels +CHANNELS+
   :output-channels +DEFAULT-CHANNELS+
   :audio-buffer-size +DEFAULT-SAMPLE-BUFFER+
   :volume +MAX-VOLUME+
   :user-data (cffi:null-pointer)
   :callback (cffi:callback default-fill-audio-buffer)))

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

  ;; Configure Lispworks to allow
  ;; callbacks from unknown foreign threads
  #+(and lispworks (not lispworks5.1))(system:setup-for-alien-threads)
 
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
    ;; Set the output buffer size.
    ;; In Windows this is not used as audio is handled
    ;; by the glue library.
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::sdl-Audio-Spec
                                   'sdl-cffi::samples)
          (requested-audio-buffer-size *mixer*))
    ;; Set the callback function
    (setf (cffi:foreign-slot-value requested-audio-spec
                                   'sdl-cffi::SDL-Audio-Spec
                                   'sdl-cffi::callback)
          (slot-value *mixer* 'callback))
  
    (if (eql -1
             #+lispbuilder-sdl-audio
             (sdl-cffi::sdl-glue-SDL-Open-Audio requested-audio-spec
                                                (sdl:fp (audio-spec *mixer*)))
             #-lispbuilder-sdl-audio
             (sdl-cffi::SDL-Open-Audio requested-audio-spec
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

(defmethod _play-audio_ ((self mixer) &key loop pos)
  (declare (ignore loop pos))
  (when (audio-opened-p)
    (sdl-cffi::sdl-pause-audio 0)
    (setf (slot-value self 'pause) nil)))

(defun play-audio (&optional obj &key loop pos)
  (unless obj
    (setf obj *mixer*))
  (_play-audio_ obj :loop loop :pos pos))

(defmethod _pause-audio_ ((self mixer))
  (when (audio-opened-p)
    (sdl-cffi::sdl-pause-audio 1)
    (setf (slot-value self 'pause) t)))

(defun pause-audio (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_pause-audio_ obj))

(defun resume-audio (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_resume-audio_ obj))

(defmethod _resume-audio_ ((self mixer))
  (play-audio self))

(defmethod _audio-paused-p_ ((self mixer))
  (when (audio-opened-p)
    (slot-value self 'pause)))

(defun audio-paused-p (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_audio-paused-p_ obj))

(defun audio-opened-p ()
  (when (and (> (sdl:return-subsystems-of-status SDL:SDL-INIT-AUDIO t) 0)
             *mixer*)
    (mixer-opened-p *mixer*)))

(defmethod _audio-playing-p_ ((self mixer))
  (when (and (audio-opened-p)
             (not (audio-paused-p self)))
    (let ((num (length *managed-audio*)))
      (if (> num 0) num nil))))

(defmethod audio-playing-p (&optional obj)
  (unless obj
    (setf obj *mixer*))
  (_audio-playing-p_ obj))

(defun close-audio ()
  ;; Pause the audio stream
  (when (audio-opened-p)
    (pause-audio)
    (setf (slot-value *mixer* 'mixer-opened-p) nil))
  ;; Lock the audio device to halt all callbacks
  #+lispbuilder-sdl-audio(sdl-cffi::sdl-glue-sdl-close-audio)
  #-lispbuilder-sdl-audio(progn
                           (sdl-cffi::sdl-lock-audio)
                           (sdl-cffi::sdl-close-audio))
  (setf *managed-audio* nil))

(defgeneric load-sample (filename))

(defmethod load-sample ((filename string))
  (let ((spec (make-instance 'audio-spec))
        (sample-handle (cffi:foreign-alloc :pointer))
        (sample-length (cffi:foreign-alloc :pointer)))
    (unless (cffi:null-pointer-p (sdl-cffi::sdl-load-wav filename
                                                         (fp spec)
                                                         sample-handle
                                                         sample-length))
      ;; Determine the format of the audio buffer
      ;; We are going to save the buffer in this format to save on storage,
      ;; and reduce bit-fiddling later on.
      (let* ((wave (cffi:mem-aref sample-handle :pointer))
             (buff (cond
                    ((= (audio-format spec) sdl-cffi::AUDIO-U8)
                     (loop repeat (cffi:mem-aref sample-length :unsigned-int)
                           for byte = 0 then (1+ byte)
                           collect (cffi:mem-aref wave :unsigned-char byte)))
                    ((= (audio-format spec) sdl-cffi::AUDIO-S8)
                     (loop repeat (cffi:mem-aref sample-length :unsigned-int)
                           for byte = 0 then (1+ byte)
                           collect (to-s8 (cffi:mem-aref wave :unsigned-char byte))))
                    ((= (audio-format spec) sdl-cffi::AUDIO-U16LSB)
                     (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                           for byte-1 = 0 then (+ 2 byte-1)
                           for byte-2 = 1 then (1+ byte-1)
                           collect (logior (ash (cffi:mem-aref wave :unsigned-char byte-2) 8)
                                           (cffi:mem-aref wave :unsigned-char byte-1))))
                    ((= (audio-format spec) sdl-cffi::AUDIO-S16LSB)
                     (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                           for byte-1 = 0 then (+ 2 byte-1)
                           for byte-2 = 1 then (1+ byte-1)
                           collect (to-s16 (logior (ash (cffi:mem-aref wave :unsigned-char byte-2) 8)
                                                   (cffi:mem-aref wave :unsigned-char byte-1)))))
                    ((= (audio-format spec) sdl-cffi::AUDIO-U16MSB)
                     (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                           for byte-1 = 0 then (+ 2 byte-1)
                           for byte-2 = 1 then (1+ byte-1)
                           collect (logior (ash (cffi:mem-aref wave :unsigned-char byte-1) 8)
                                           (cffi:mem-aref wave :unsigned-char byte-2))))
                    ((= (audio-format spec) sdl-cffi::AUDIO-S16MSB)
                     (loop repeat (/ (cffi:mem-aref sample-length :unsigned-int) 2)
                           for byte-1 = 0 then (+ 2 byte-1)
                           for byte-2 = 1 then (1+ byte-1)
                           collect (to-s16 (logior (ash (cffi:mem-aref wave :unsigned-char byte-1) 8)
                                           (cffi:mem-aref wave :unsigned-char byte-2))))))))
        (sdl-cffi::sdl-free-wav wave)
        (cffi:foreign-free sample-handle)
        (cffi:foreign-free sample-length)
        (make-instance 'audio
                       :audio-buffer-handle
                       (make-instance 'audio-buffer
                                      :audio-buffer (make-array
                                                     (length buff)
                                                     :element-type (from-sdl-type
                                                                    (audio-format spec))
                                                     :initial-contents buff)
                                      :audio-length (length buff)
                                      :audio-spec spec))))))

(defmethod load-sample ((filename pathname))
  (load-sample (namestring filename)))

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

        ;; Write the AUDIO to the SDL-AUDIO-CVT buffer for conversion.
        ;; The SDL-AUDIO-CVT takes uint* buffer, so perform coversion
        ;; if necessary.
        (cond
         ((= (audio-format audio) sdl-cffi::AUDIO-U8)
          (loop for val across (audio-buffer audio)
                for i = 0 then (1+ i) do
                (setf (cffi:mem-aref sdl-cffi::buf :unsigned-char i) val)))
         ((= (audio-format audio) sdl-cffi::AUDIO-S8)
          (loop for val across (audio-buffer audio)
                for i = 0 then (1+ i) do
                (setf (cffi:mem-aref sdl-cffi::buf :unsigned-char i)
                      (from-s8 val))))
         ((= (audio-format audio) sdl-cffi::AUDIO-U16LSB)
          (loop for val across (audio-buffer audio)
                for x = 0 then (+ x 2)
                for y = 1 then (+ x 1) do
                (setf (mem-aref sdl-cffi::buf :unsigned-char x) (logand val #xFF)
                      (mem-aref sdl-cffi::buf :unsigned-char y) (logand (ash val -8)
                                                                 #xFF))))
         ((= (audio-format audio) sdl-cffi::AUDIO-S16LSB)
          (loop for val across (audio-buffer audio)
                for x = 0 then (+ x 2)
                for y = 1 then (+ x 1)
                for uval = (from-s16 val) do
                (setf (mem-aref sdl-cffi::buf :unsigned-char x) (logand uval #xFF)
                      (mem-aref sdl-cffi::buf :unsigned-char y) (logand (ash uval -8)
                                                                 #xFF))))
         ((= (audio-format audio) sdl-cffi::AUDIO-U16MSB)
          (loop for val across (audio-buffer audio)
                for x = 0 then (+ x 2)
                for y = 1 then (+ x 1) do
                (setf (mem-aref sdl-cffi::buf :unsigned-char y) (logand val #xFF)
                      (mem-aref sdl-cffi::buf :unsigned-char x) (logand (ash val -8)
                                                                 #xFF))))
         ((= (audio-format audio) sdl-cffi::AUDIO-S16MSB)
          (loop for val across (audio-buffer audio)
                for x = 0 then (+ x 2)
                for y = 1 then (+ x 1)
                for uval = (from-s16 val) do
                (setf (mem-aref sdl-cffi::buf :unsigned-char y) (logand uval #xFF)
                      (mem-aref sdl-cffi::buf :unsigned-char x) (logand (ash uval -8)
                                                                 #xFF)))))
        
        ;; Need to fix this to convert from source format to destination format.
        ;; Assumes source is uint8 right now.
        ;(dotimes (i (audio-length audio))
        ;  (setf (cffi:mem-aref sdl-cffi::buf :unsigned-char i)
        ;        (aref (audio-buffer audio) i)))
        
        (when (> (sdl-cffi::sdl-convert-audio audio-cvt) -1)
          (let* ((cvt-buffer
                  (make-pointer
                   (pointer-address (foreign-slot-value audio-cvt
                                                        'sdl-cffi::sdl-audio-cvt
                                                        'sdl-cffi::buf))))
                 (buffer
                  (cond
                   ((= (audio-format mixer) sdl-cffi::AUDIO-U16LSB)
                    (loop repeat (calculated-buffer-length
                                  sdl-cffi::len-cvt
                                  sdl-cffi::AUDIO-U8
                                  (audio-format mixer))
                          for byte-1 = 0 then (+ 2 byte-1)
                          for byte-2 = 1 then (1+ byte-1)
                          collect (logior (ash (cffi:mem-aref cvt-buffer
                                                              :unsigned-char byte-2) 8)
                                          (cffi:mem-aref cvt-buffer
                                                         :unsigned-char byte-1))))
                   ((= (audio-format mixer) sdl-cffi::AUDIO-S16LSB)
                    (loop repeat (calculated-buffer-length
                                  sdl-cffi::len-cvt
                                  sdl-cffi::AUDIO-U8
                                  (audio-format mixer))
                          for byte-1 = 0 then (+ 2 byte-1)
                          for byte-2 = 1 then (1+ byte-1)
                          collect (to-s16 (logior (ash (cffi:mem-aref cvt-buffer
                                                                      :unsigned-char byte-2) 8)
                                                  (cffi:mem-aref cvt-buffer
                                                                 :unsigned-char byte-1)))))
                   ((= (audio-format mixer) sdl-cffi::AUDIO-U16MSB)
                    (loop repeat (calculated-buffer-length sdl-cffi::len-cvt
                                                           sdl-cffi::AUDIO-U8
                                                           (audio-format mixer))
                          for byte-1 = 0 then (+ 2 byte-1)
                          for byte-2 = 1 then (1+ byte-1)
                          collect (logior (ash (cffi:mem-aref cvt-buffer
                                                              :unsigned-char byte-1) 8)
                                          (cffi:mem-aref cvt-buffer
                                                         :unsigned-char byte-2))))
                   ((= (audio-format mixer) sdl-cffi::AUDIO-S16MSB)
                    (loop repeat (calculated-buffer-length sdl-cffi::len-cvt
                                                           sdl-cffi::AUDIO-U8
                                                           (audio-format mixer))
                          for byte-1 = 0 then (+ 2 byte-1)
                          for byte-2 = 1 then (1+ byte-1)
                          collect (to-s16 (logior (ash (cffi:mem-aref cvt-buffer
                                                                      :unsigned-char byte-1) 8)
                                                  (cffi:mem-aref cvt-buffer
                                                                 :unsigned-char byte-2)))))
                   ((= (audio-format mixer) sdl-cffi::AUDIO-U8)
                    (loop repeat (calculated-buffer-length
                                  sdl-cffi::len-cvt
                                  sdl-cffi::AUDIO-U8
                                  (audio-format mixer))
                          for byte = 0 then (1+ byte)
                          collect (cffi:mem-aref cvt-buffer
                                                 :unsigned-char byte)))
                   ((= (audio-format mixer) sdl-cffi::AUDIO-S8)
                    (loop repeat (calculated-buffer-length
                                  sdl-cffi::len-cvt
                                  sdl-cffi::AUDIO-U8
                                  (audio-format mixer))
                          for byte = 0 then (1+ byte)
                          collect (to-s8 (cffi:mem-aref cvt-buffer
                                                 :unsigned-char byte)))))))
            (sdl-cffi::sdl-free-wav
             (make-pointer
              (pointer-address
               (foreign-slot-pointer audio-cvt
                                     'sdl-cffi::sdl-audio-cvt
                                     'sdl-cffi::buf))))
            (make-instance 'audio
                           :audio-buffer-handle
                           (make-instance 'audio-buffer
                                          :audio-buffer
                                          (make-array (length buffer)
                                                      :element-type (from-sdl-type
                                                                     (audio-format mixer))
                                                      :initial-contents buffer)
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
  (when (audio-opened-p)
    (let ((audio-buffer (load-sample filename)))
    (when (and audio-buffer (audio-opened-p))
      (build-audio-cvt audio-buffer *mixer*)))))

(defun fill-audio-buffer (stream len)
  (unless (slot-value *mixer* 'pause)
    ;; The callback is asking for an amount of 'len' uint8 bytes.
    ;; Have to convert this to the mixer format, as this is what all the audio
    ;; buffers will be in.
    ;;(format t "stream: ~A, len: ~A~%" stream len)
    ;;(force-output t)
    (let ((len (calculated-buffer-length len sdl-cffi::audio-u8
                                         (audio-format *mixer*))))
      ;; If the callback function is asking for more data than the existing output
      ;; buffer, then create a new output buffer of the requested length.
      (if (/= len (length (output-buffer *mixer*)))
        (setf (output-buffer *mixer*)
              (make-array len
                          :initial-element 
                          (audio-silence (audio-spec *mixer*))
                          :element-type 'fixnum))
        ;; Write silence to the output buffer
        (overwrite-all (output-buffer *mixer*)
                       (audio-silence (audio-spec *mixer*))))
      (let ((output (output-buffer *mixer*)))
        (dolist (audio *managed-audio*)
          (unless (slot-value audio 'pause)
            ;; Play audio only when audio remains in the sample.
            (fill-output-buffer output audio)))

        ;; Removed from *managed-audio* list if sample is finished.
        (dolist (obj (remove nil (loop for buffer in *managed-audio*
                                       collect (remove-audio-p
                                                buffer))))
          (setf *managed-audio* (remove obj *managed-audio*)
                (remove-audio-p obj) nil))

        ;; Set mixer volume limits.
        (let ((master-volume (audio-volume *mixer*))
              (max-audioval (if (= 2 (element-size (audio-format *mixer*)))
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
                      (mem-aref stream :unsigned-char y) (logand (ash clamped -8) #xFF))))))))

;(defun process-audio ()
;  (if *mixer*
;    (progn
;      (format t "In PROCESS-AUDIO~%")
;      (let ((fill? (sdl-cffi::SDL-glue-SDL-Require-Buffer-Fill)))
;        (format t "  fill?: ~A : " fill?)
;        (if (= fill? 1)
;          (progn
;            (format t "Require Buffer Fill : ")
;            (let ((stream (sdl-cffi::SDL-glue-SDL-Get-Audio-Buffer))
;                  (len (sdl-cffi::SDL-glue-SDL-Get-Audio-Buffer-length)))
;              (format t "len == ~A~%" len)
;              (fill-audio-buffer stream
;                                 len)
;              (sdl-cffi::SDL-glue-SDL-Buffer-Filled))
;            (format t "Buffer Filled~%~%")
;            (force-output t))
;          (progn
;            (format t "Buffer Full~%~%")
;            (force-output t)))))
;      (format t "*mixer* not yet initialized~%")))

#+lispbuilder-sdl-audio
(defun process-audio ()
  (when (and *mixer*
             (= (sdl-cffi::SDL-glue-SDL-Require-Buffer-Fill) 1))
    (fill-audio-buffer (sdl-cffi::SDL-glue-SDL-Get-Audio-Buffer)
                       (sdl-cffi::SDL-glue-SDL-Get-Audio-Buffer-length))
    (sdl-cffi::SDL-glue-SDL-Buffer-Filled)))

(cffi:defcallback default-fill-audio-buffer
    :pointer ((user-data :pointer)
              (stream :pointer)
              (len :int))
  (declare (ignore user-data))
  (fill-audio-buffer stream len)
  (cffi:null-pointer))
