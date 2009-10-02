
(in-package #:sdl-examples)

(defun display-test (text pos &key (font sdl:*default-font*) (surface sdl:*default-display*) (color sdl:*default-color*))
  (let ((text (if (listp text) text (list text)))
        (between 2))
    (loop for line in text
          for i = 0 then (incf i)
          do (sdl:draw-string-solid line (vector (sdl:x pos) (+ (sdl:y pos )
                                                                (* i (+ between (sdl:get-font-size line :size :H)))))
                                    :surface surface
                                    :font font
                                    :color color))))

(defun active-event ()
  (let ((text ""))
    (sdl:with-init ()
      (sdl:window 320 200 :title-caption ":ACTIVE-EVENT" :icon-caption "ACTIVE-EVENT")
      (sdl:initialise-default-font)
      (setf (sdl:frame-rate) 10)
      (sdl:with-events ()
        (:active-event (:gain gain :state state)
         (when (sdl:mouse-focus-p state)
           (if (sdl:mouse-gain-focus-p gain state)
               (setf text "Event: Window gained mouse focus")
               (setf text "Event: Window lost   mouse focus"))))
        (:key-down-event ()
         (when (sdl:key-down-p :sdl-key-escape)
           (sdl:push-quit-event)))
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:idle ()
         (sdl:clear-display sdl:*black*)
         (sdl:draw-string-solid (if (sdl:mouse-focus-p)
                                  "State: Mouse over Window"
                                  "State: Mouse left Window")
                                #(10 10) :surface sdl:*default-display* :color sdl:*white*)
         (sdl:draw-string-solid text #(10 20) :surface sdl:*default-display* :color sdl:*white*)

         (display-test (list "Note: ':ACTIVE-EVENTs' are not received"
                                      "      in win32 following a"
                                      "      SDL:PUSH-QUIT-EVENT."
                                      "      Seems to be a SDL library issue.")
                       #(10 40))
         (sdl:update-display))))))

