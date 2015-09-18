;;; This is a test file just to see if I can get clisp with
;;; slime to compile and load something properly on my machine

;;; File: slime-test.lisp

(defun wtf (arg1)
  (dotimes (i 5 nil)
    (print arg1)))