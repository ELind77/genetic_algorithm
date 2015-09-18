;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  create-population-2.lisp
;;; ========================================

;;; CREATE-INDI-PROG
;;; CREATE-POPULATION
;;; EVALUATE-POP-FITNESS
;;; SORT-POP-BY-FITNESS


;;;   TO-DO!!!
;;;************************************************************
;;;  1. "Grown" trees for indi-progs
;;;  2. "Ramp-up" for indi-progs/create-pop
;;;  3. Top-node-p for create-indi-prog
;;;  4. FITNESS-CASES
;;;  5. Normalized fitness???
;;;  6. "Uniquify" initial generation
;;;  7. Breed new POP
;;;    a. Select individual
;;;    b. COPY-TREE
;;;    c. GET-SUBTREE
;;;    d. COUNT-CROSSOVER-POINTS
;;;  8. Mutation?


;;;   DONE
;;;**************************************************
;;;  9. PREVENT DIVISION BY ZERO!!!!!


;;;************************************************************
;;;   GLOBALS
;;;************************************************************

;;; *POPULATION*
;;;--------------------------------------
(defvar *population* nil)

;;; POP-SIZE
;;;----------------------------------------
;;(defvar *pop-size* 100)

;;; *HEIGHT*
;;;----------------------------------------
(defvar *height* 3)


;;;************************************************************
;;;  PROTECTED FUNCTIONS
;;;************************************************************


;;; %
;;;============================================================
;;; The protected division function. Returns 1 if user attempts
;;; to divide by zero.

(defun % (numerator denominator)
  "The protected division function"
  (if (= 0 denominator)
      1
      (/ numerator denominator)))





;;;************************************************************
;;;   FUNCTIONS
;;;************************************************************


;;;============================================================
;;;  INDIVIDUAL
;;;============================================================
(defstruct individual
  program
  (fitness 0))


;;;============================================================
;;;  CREATE-INDI-PROG
;;;============================================================
;;; Recursively create a function tree from the given function
;;; set and terminal set.
;;;
;;; For this implementation, the function will only
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
;;;           FULL?, boolean, are we creating full trees or 
;;;            grown trees?
;;;           TOP-NODE?, boolean, T if being called from the root node
;;;            and NIL otherwise. This lets the function be used to
;;;            create parts of trees for mutation.
;;;  OUTPUT:  A new function composed of elements of the given
;;;           sets

(defun create-indi-prog (function-set argument-map 
			 terminal-set allowable-depth 
			 &optional (full? t) (top-node? t))
  
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


;;;============================================================
;;;  CREATE-POPULATION
;;;============================================================
;;; Iteratively create the population for testing
;;;
;;;  INPUTS:   POP-SIZE
;;;            FUNCTION-SET, a set of functions
;;;            ARGUMENT-MAP, the number of argumnets each element
;;;             of the FUNCTION-SET takes
;;;            TERMINAL-SET, a set of terminals
;;;  OUTPUT:   An array of individuals

(defun create-population (pop-size function-set argument-map terminal-set)
 "Creates the population.  The population is an array of size
 POP-SIZE."
 
 ;; Initialize the population vector
 (let ((population (make-array pop-size)))
   ;; Loop through the pop vector and setf the PROGS
   (dotimes (i pop-size population)
     (let ((prog (create-indi-prog function-set argument-map
				   terminal-set *height*)))
       (setf (svref population i)
	 (make-individual
	  :program prog))))
   
   ;;(setf *population* population)
   ))


;;;============================================================
;;;  EVALUATE-POP-FITNESS
;;;============================================================
;;; Evaluates the fitness of every individual in a population.
;;;
;;;  INPUTS:  POPULATION, a vector of INDIVUDUALS
;;;           FITNESS-CASES, for this version there is only one
;;;           FITNESS-FXN, just EVAL in this version.
;;;  OUTPUT:  The POPULATION vector with fitnesses

(defun evaluate-pop-fitness (population fitness-fxn &optional (fitness-cases 1))
  "Loops through the population vector and evaluates the fitness 
   of each"
  
  (dotimes (i (length population) population)
    (let* ((indi (svref population i))
	   (indi-prog (individual-program indi))
	   (indi-fitness (funcall fitness-fxn indi-prog)))
      
      ;; Set the fitness of each INDI
      (setf (individual-fitness indi) indi-fitness))))


;;;============================================================
;;;  SORT-POP-BY-FITNESS
;;;============================================================
;;; Destructively sorts the population according to fitness
;;;
;;;  INPUTS:  POPULATION, a vector of INDIVUDUALS
;;;  OUTPUT:  The POPULATION vector sorted by fitness

(defun sort-pop-by-fitness (population)
  "Sorts the population by (normalized) fitness.  Destructively."
  
  (sort population #'> :key #'individual-fitness))











;;;************************************************************
;;;   TESTS
;;;************************************************************

(defvar *f-set* '(+ - * %))

(defvar *a-map* '(2 2 2 2))

(defvar *t-set* '(2 3 4 6))

(defvar *pop-size* 10)


;;; TESTS for CREATE-POPULATION
;;;--------------------------------
(print-test-header "CREATE-POPULATION")
;;(tester '(create-population *pop-size* *f-set* *a-map* *t-set*))

(defvar *test-pop* (create-population *pop-size* *f-set* *a-map* *t-set*))



;;; TESTS for EVALUATE-POP-FITNESS
;;;--------------------------------
(print-test-header "EVELUATE-POP-FITNESS")
;;(tester '(evaluate-pop-fitness *test-pop* #'eval))

(defvar *test-evaluated-pop* (evaluate-pop-fitness *test-pop* #'eval))


;;; TESTS for SORT-POP-BY-FITNESS
;;;-------------------------------
(print-test-header "SORT-POP-BY-FITNESS")
(tester '(sort-pop-by-fitness *test-evaluated-pop*))