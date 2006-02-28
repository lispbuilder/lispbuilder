(let ((bob 3))
  (defun a-function(x)
    "This function has a multiline comment, which
spans multiple lines of 
lisp code."
    (setf bob (+ bob x)))
  (defun b-function()
    bob))



(defmacro with-my-thing(thing &body body)
  `(progn
    (a-function ,thing)
    ,@body
    (a-function ,thing)))

