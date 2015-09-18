;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  test-gp-system.lisp
;;; ========================================
;;; File containing tests and functions to run the 
;;; genetic-programming system for the stock prediction
;;; problem.


;;; Stuff to Load
;;;==========================================================

;; Kernel
(load (compile-file "kernel-5"))

;; Randomizer functions
(load (compile-file "randomizer"))

;; Reporting functions
(load (compile-file "reporters"))

;; Problem-Specific Code
(load (compile-file "problem-code-2"))



;;;************************************************************
;;;   TEST FUNCTIONS
;;;************************************************************

;;; Test functions
(defun test1 ()
  ;; Clear out *DATA-ARRAY*
  (setf *data-array* nil)
  
  ;; Create the *DATA-ARRAY*
  (create-data-array 31 111 10)
  ;; Run GP
  (run-gp-system 'stock-prediction 1.0 50 200))


(defun test2 (start end num-cases &optional (max-gens 50) (pop-size 200))
  (progn
    ;; Clear out *DATA-ARRAY*
    (setf *data-array* nil) 
    ;; Create the *DATA-ARRAY*
    (create-data-array start end num-cases)
    ;; Run GP
    (run-gp-system 'stock-prediction 1.0 max-gens pop-size)
    ;; Print BEST-OF-RUN
    *best-of-run-individual*))


