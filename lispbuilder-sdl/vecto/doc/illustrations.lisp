;;;; $Id: illustrations.lisp,v 1.6 2007/10/01 16:24:10 xach Exp $

(defpackage #:vecto-illustrations
  (:use #:cl #:vecto))

(in-package #:vecto-illustrations)

(defun x (point)
  (car point))

(defun y (point)
  (cdr point))

(defun annotated-path (&rest points)
  (with-graphics-state
    (set-rgb-stroke 0.5 0.5 0.5)
    (set-rgb-fill 0.5 0.5 0.5)
    (set-line-width 2)
    (dolist (point (remove-duplicates points :test 'equal))
      (centered-circle-path (x point) (y point) 3))
    (fill-path)
    (move-to (x (first points)) (y (first points)))
    (dolist (point (rest points))
      (line-to (x point) (y point)))
    (stroke)))


(defun join-style (style file)
  (with-canvas (:width 160 :height 165)
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-stroke 0 0 0)
    (set-line-width 20)
    (move-to 20 20)
    (line-to 80 140)
    (line-to 140 20)
    (set-line-join style)
    (stroke)
    (annotated-path '(20 . 20)
                    '(80 . 140)
                    '(140 . 20))
    (save-png file)))


(defun cap-style (style file)
  (with-canvas (:width 40 :height 100)
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-stroke 0 0 0)
    (set-line-width 20)
    (move-to 20 20)
    (line-to 20 80)
    (set-line-cap style)
    (stroke)
    (annotated-path '(20 . 20) '(20 . 80))
    (save-png file)))



(defun closed-subpaths (closep file)
  (with-canvas (:width 160 :height 160)
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-stroke 0 0 0)
    (set-line-width 20)
    (move-to 20 20)
    (line-to 20 140)
    (line-to 140 140)
    (line-to 140 20)
    (line-to 20 20)
    (when closep
      (close-subpath))
    (stroke)
    (annotated-path '(20 . 20)
                    '(20 . 140)
                    '(140 . 140)
                    '(140 . 20)
                    '(20 . 20))
    (save-png file)))

(defun dash-paths (array phase cap-style file)
  (with-canvas (:width 160 :height 40)
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-stroke 0 0 0)
    (set-line-width 20)
    (with-graphics-state
      (set-dash-pattern array phase)
      (set-line-cap cap-style)
      (move-to 20 20)
      (line-to 140 20)
      (stroke))
    (annotated-path '(20 . 20) '(140 . 20))
    (save-png file)))
  

(defun simple-clipping-path (file &key clip-circle clip-rounded-rectangle)
  (with-canvas (:width 100 :height 100)
    (let ((x0 45)
          (y 45)
          (r 40))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (with-graphics-state
        (set-rgb-fill 0.9 0.9 0.9)
        (rectangle 10 10 80 80)
        (fill-path))
      (with-graphics-state
        (when clip-circle
          (centered-circle-path x0 y r)
          (clip-path)
          (end-path-no-op))
        (when clip-rounded-rectangle
          (rounded-rectangle 45 25 50 50 10 10) 
          (clip-path)
          (end-path-no-op))
        (set-rgb-fill 1 0 0)
        (set-rgb-stroke 1 1 0)
        (rectangle 10 10 80 80)
        (fill-path))
      (when clip-circle
        (with-graphics-state
          (set-rgb-stroke 0.5 0.5 0.5)
          (set-dash-pattern #(5) 0)
          (set-line-width 1)
          (centered-circle-path x0 y r)
          (stroke)))
      (when clip-rounded-rectangle
        (with-graphics-state
          (set-rgb-stroke 0.5 0.5 0.5)
          (set-dash-pattern #(5) 0)
          (set-line-width 1)
          (rounded-rectangle 45 25 50 50 10 10) 
          (stroke)))
      (save-png file))))
  

(defun make-illustrations ()
  (cap-style :butt "cap-style-butt.png")
  (cap-style :square "cap-style-square.png")
  (cap-style :round "cap-style-round.png")
  (join-style :miter "join-style-miter.png")
  (join-style :bevel "join-style-bevel.png")
  (join-style :round "join-style-round.png")
  (closed-subpaths nil "open-subpath.png")
  (closed-subpaths t "closed-subpath.png")
  (dash-paths #() 0 :butt "dash-pattern-none.png")
  (dash-paths #(30 30) 0 :butt "dash-pattern-a.png")
  (dash-paths #(30 30) 15 :butt "dash-pattern-b.png")
  (dash-paths #(10 20 10 40) 0 :butt "dash-pattern-c.png")
  (dash-paths #(10 20 10 40) 13 :butt "dash-pattern-d.png")
  (dash-paths #(30 30) 0 :round "dash-pattern-e.png")
  (simple-clipping-path "clip-unclipped.png")
  (simple-clipping-path "clip-to-circle.png" :clip-circle t)
  (simple-clipping-path "clip-to-rectangle.png" :clip-rounded-rectangle t)
  (simple-clipping-path "clip-to-both.png"
                        :clip-circle t
                        :clip-rounded-rectangle t))
