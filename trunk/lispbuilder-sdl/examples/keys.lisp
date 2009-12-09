
(in-package #:sdl-examples)

(defun key-test ()
  (sdl:with-init ()
    (sdl:window 800 400
                :title-caption "Key Test" :icon-caption "Key Test")
    (sdl:initialise-default-font)
    (let ((key-state nil) (key-scancode nil) (key-key nil) (key-mod nil) (key-unicode nil) (key-mod-key nil)
          (key-press-status nil)
          (y-pos 0))
    
      (sdl:enable-unicode)

      (sdl:with-events ()
        (:quit-event () t)        
        (:key-down-event (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE)
         (setf key-press-status :key-down-event
               key-state state
               key-scancode scancode
               key-key key
               key-mod mod
               key-mod-key mod-key
               key-unicode unicode))
        (:key-up-event (:STATE STATE :SCANCODE SCANCODE :KEY KEY :MOD MOD :MOD-KEY MOD-KEY :UNICODE UNICODE)
         (setf key-press-status :key-up-event
               key-state state
               key-scancode scancode
               key-key key
               key-mod mod
               key-mod-key mod-key
               key-unicode unicode))
        (:idle ()
         (sdl:clear-display sdl:*black*)
         
         (setf y-pos 0)

         (sdl:draw-string-solid-* (format nil "(UNICODE-ENABLED-P) => ~A" (sdl:unicode-enabled-p))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(KEY-REPEAT-DELAY) => ~A" (sdl:key-repeat-delay))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(KEY-REPEAT-INTERVAL) => ~A" (sdl:key-repeat-interval))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(KEY-STATE-P) => ~A"
                                          (sdl:key-state-p))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(KEY-STATE-P :SDL-KEY-SPACE) => ~A"
                                          (sdl:key-state-p :sdl-key-space))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(MOD-STATE-P) => ~A"
                                          (sdl:mod-state-p))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         (sdl:draw-string-solid-* (format nil "(MOD-STATE-P :SDL-KEY-MOD-LCTRL) => ~A"
                                          (sdl:mod-state-p :SDL-KEY-MOD-LCTRL))
                                  10 (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*)
         
         (sdl:draw-string-solid-* (format nil "--- Previous Received Event: ~A ---"
                                          (if key-press-status
                                            key-press-status
                                            "Press Any Key"))
                                  (sdl:cast-to-int
                                   (/ (sdl:width sdl:*default-display*) 2))
                                  (incf y-pos 15)
                                  :color sdl:*white*
                                  :surface sdl:*default-display*
                                  :justify :center)
         
         (when key-press-status
           (sdl:draw-string-solid-* (format nil "STATE: ~A" key-state)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "SCANCODE: ~A" key-scancode)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "KEY: ~A" key-key)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "MOD: ~A" key-mod)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "MOD-KEY: ~A" key-mod-key)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "UNICODE: ~A" key-unicode)
                                    10 (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "(CODE-CHAR unicode): ~S" (code-char key-unicode))
                                    140 y-pos
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)

           (sdl:draw-string-solid-* (format nil "--- --- ---")
                                    (sdl:cast-to-int
                                     (/ (sdl:width sdl:*default-display*) 2))
                                    (incf y-pos 15)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*
                                    :justify :center)
           
           (sdl:draw-string-solid-* (format nil "(MODIFIER-P MOD) => ~A"
                                            (sdl:modifier-p key-mod))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "(MODIFIER-P MOD-KEY) => ~A"
                                            (sdl:modifier-p key-mod-key))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "(MODIFIER-P MOD :SDL-KEY-MOD-LCTRL) => ~A"
                                            (sdl:modifier-p key-mod :SDL-KEY-MOD-LCTRL))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)           
           (sdl:draw-string-solid-* (format nil "(MODIFIER-P MOD-KEY :SDL-KEY-MOD-LCTRL) => ~A"
                                            (sdl:modifier-p key-mod-key :SDL-KEY-MOD-LCTRL))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           (sdl:draw-string-solid-* (format nil "(MODIFIER= '(:SDL-KEY-MOD-LCTRL :SDL-KEY-MOD-LALT) MOD) => ~A"
                                            (sdl:modifier= '(:sdl-key-mod-lctrl :sdl-key-mod-lalt)
                                                           key-mod))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*)
           
           (sdl:draw-string-solid-* (format nil "(MODIFIER= '(:SDL-KEY-MOD-LCTRL :SDL-KEY-MOD-LALT) MOD-KEY) => ~A"
                                            (sdl:modifier= '(:sdl-key-mod-lctrl :sdl-key-mod-lalt)
                                                           key-mod-key))
                                    10 (incf y-pos 20)
                                    :color sdl:*white*
                                    :surface sdl:*default-display*))
         (sdl:update-display))))))
