;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  create-indi-prog-1.lisp
;;; ========================================

;;; This file is an early version of the function for
;;; creating the functions for individuals in a population. 
;;; It may or may not function properly on its own.


;;;============================================================
;;;  CREATE-INDI-PROG
;;;============================================================
;;; Recursively create a function tree from the given function
;;; set and terminal set.
;;;
;;; For this first implementation, the function will only
;;; create "full" trees.
;;;
;;; This implementation also assumes all functions take 2
;;; arguments.
;;;
;;;  INPUTS:  FUNCTION-SET, a set of functions
;;;           TERMINAL-SET, a set of terminals
;;;           ALLOWABLE-DEPTH, the remaining depth of the tree 
;;;            to be created.
;;;  OUTPUT:  A new function composed of elements of the given
;;;           set

(defun create-indi-prog (function-set terminal-set allowable-depth)
  
  (labels
      ;; CHOOSE-TERMINAL
      ;;--------------------------
      ;; Returns a random element from TERM-SET
      ;;  INPUTS:  TERM-SET
      ;;  OUTPUT:  an element of TERM-SET
      ((choose-terminal (term-set)
	 (let ((rand (random (length term-set))))
	   ;; Return a random element from TERM-SET
	   (elt term-set rand)))
       
       ;; CHOOSE-FUNCTION
       ;;---------------------------
       ;; Chooses a random function from FUNC-SET
       ;; INPUTS:  FUNC-SET
       ;; OUTPUT:  an element of FUNC-SET
       (choose-function (func-set)
	 (let ((rand (random (length func-set))))
	   ;; Return random elt
	   (elt func-set rand))))
  
    (cond
     ;; BC
     ((<= allowable-depth 0)
      ;; We're are the maximum depth so return a terminal
      (choose-terminal terminal-set))
     
     ;; Otherwise, choose a function
     (t
      (let ((fxn (choose-function function-set)))
	
	(cons fxn
	      ;; Arg 1
	      (cons
	       (create-indi-prog 
		function-set 
		terminal-set 
		(- allowable-depth 1))
	       ;; Arg 2
	       (cons
		(create-indi-prog
		 function-set
		 terminal-set
		 (- allowable-depth 1))
		nil))))))))
		 
   
   
