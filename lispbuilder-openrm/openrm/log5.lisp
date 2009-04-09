
(in-package #:rm)

(log5:defcategory create)
(log5:defcategory free)
(log5:defcategory manual-free)
(log5:defcategory gc)
(log5:defcategory tg)
(log5:defcategory info)

;(log5:start-sender 'general-logging
; 		   (log5:stream-sender :location "openrm.log")  
; 		   :category-spec '(create free manual-free gc tg info)  
;                   :output-spec '(log5:time log5:message))

