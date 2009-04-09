
(in-package #:lispbuilder-openrm)

;; These are the functions called in the
;; win32 WINDOW-PROC event handler.
(defvar *win-idle-function* nil)
(defvar *win-mouse-click-handler* nil)
(defvar *win-mouse-move-handler* nil)
(defvar *win-mouse-up-handler* nil)
(defvar *win-mouse-down-handler* nil)

(defvar *timers* nil)