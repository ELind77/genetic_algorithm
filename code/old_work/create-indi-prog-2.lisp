;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  create-indi-prog-2.lisp
;;; ========================================

;;; This version of CREATE-INDI-PROG can handle functions
;;; that take any number of arguments.


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
;;;           ARGUMENT-MAP, the number of argumnets each element
;;;            of the FUNCTION-SET takes
;;;           TERMINAL-SET, a set of terminals
;;;           ALLOWABLE-DEPTH, the remaining depth of the tree 
;;;            to be created.
;;;  OUTPUT:  A new function composed of elements of the given
;;;           set

(defun create-indi-prog (function-set argument-map terminal-set allowable-depth)
  
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
       
       ;; CHOOSE-FUNCTION  (DEPRICATED)                <<=============
       ;;---------------------------
       ;; Chooses a random function from FUNC-SET
       ;; INPUTS:  FUNC-SET
       ;; OUTPUT:  an element of FUNC-SET
       (choose-function (func-set)
	 (let ((rand (random (length func-set))))
	   ;; Return random elt
	   (elt func-set rand)))
       
       
       ;; CREATE-FUNCTION-ARGS
       ;;----------------------------
       ;; Creates the argument list for a function node in the tree
       ;; INPUTS:   NUM-ARGS
       ;;           FUNC-SET
       ;;           ARG-MAP
       ;;           TERM-SET
       ;;           AL-DEP
       ;; OUTPUT:   Arguments for a function
       
       (create-function-args (num-args func-set arg-map term-set al-dep)
	 (if (= num-args 0)
	     ;; If there are no args left return NIL
	     nil
	   ;; Else: generate an ARG
	   (cons 
	    ;; Each ARG is a new branch
	    (create-indi-prog func-set arg-map term-set al-dep)
	    ;; The next ARG is the next part of the CONS
	    (create-function-args (- num-args 1)
				  func-set arg-map term-set
				  al-dep)))))
    
    ;; End of LABELS
    ;;-----------------      
  
    (cond
     ;; BC
     ((<= allowable-depth 0)
      ;; We're are the maximum depth so return a terminal
      (choose-terminal terminal-set))
     
     ;; Otherwise, choose a function
     (t
      (let* ((rand (random (length function-set)))
	     (fxn (elt function-set rand))
	     (num-as (elt argument-map rand)))
	;; CONS FXN to ARGS
	(cons fxn
	      ;; ARGS
	      (create-function-args num-as 
				    function-set
				    argument-map 
				    terminal-set 
				    (- allowable-depth 1))))))))
		 
   
   
