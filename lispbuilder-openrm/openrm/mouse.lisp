
(in-package #:lispbuilder-openrm)

(defgeneric translate-mouse-button (button))
(defgeneric mouse-button-p (state button))

;; (defmethod translate-mouse-button ((window window) state id)
;;   nil)
;;   (let ((button (case id
;; 		  (:right +mouse-right-button+)
;; 		  (:middle +mouse-middle-button+)
;; 		  (:left +mouse-left-button+)
;; 		  (otherwise 0))))
;;     (when (> (logand state button) 0)
;;       id))

