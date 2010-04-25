;;; -*- lisp -*-

(in-package #:cl-user)

(defpackage #:lispbuilder-openrm
  (:use #:cl #:cffi #:simfin)
  (:nicknames #:rm)
  (:documentation "The main package of `lispbuilder-openrm'.")
  (:export

   ;; "globals.lisp"
   #:*current-pipe*
   #:*rm-root-node*
   #:*root-node*
 
   ;; "util.lisp"
   #:is-valid-ptr
   #:to-radian
   #:to-degree

   ;; "generics.lisp"
   #:free
   #:width
   #:height
   
   ;; "init"
   #:with-init
   
   ;; "pipe.lisp"
   #:pipe
   #:new-pipe
   #:current-pipe
   
   ;; "node.lisp"
   #:node
   #:new-node
   #:root-node
   #:get-root-node
   #:add-child-node
   #:add-primitive

   ;; "primitive.lisp"
   #:new-sphere-primitive

   ;; "color.lisp"
   #:color

   ;; "vertex.lisp"
   #:vertex
   
   ;; "rendering.lisp"
   #:render-frame
   #:render

   ;; "window.lisp"
   #:window
   #:current-window

   ;; "scene.lisp"
   #:scene
   #:viewport
   #:add-scene
   #:camera-2d
   #:camera-3d
   #:with-camera
   #:set-defaults
   #:set-scene-defaults
   ))
