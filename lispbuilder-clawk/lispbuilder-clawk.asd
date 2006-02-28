;;; -*-Lisp-*-

(asdf:defsystem #:lispbuilder-clawk
  :name "lispbuilder-clawk"
  :author "Michael Parker <mparker762@hotmail.com>"
  :licence "BSD"
  :description "The Unix AWK language, albeit with a pretty lisp-y flavor"
  
  :components 
  ((:file "packages")
   (:file "clawk" :depends-on ("packages"))
   (:file "clawktest" :depends-on ("packages" "clawk"))))
