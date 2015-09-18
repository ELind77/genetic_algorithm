;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  kernel.lisp
;;; ========================================
;;; This file contains a collection of generic LISP
;;; functions that can be applied to any genetic programming
;;; problem.  

;;; ACKNOWLEDGMENT:
;;;----------------------------------------------------------------
;;;The basis for these functions was the simple
;;; LISP code in John R. Koza's book: Genetic Programming:
;;; On the Programming of Computers by Means of Natural Selection.
;;; The simple LISP kernel is in Appendix C, and has been approved
;;; for educational and personal use.
;;;----------------------------------------------------------------

;;; Notes on this version
;;;----------------------------------------------------------
;;; This version of the kernel has some print functions 
;;; removed from the RUN-GP-SYSTEM and EXECUTE-GENERATIONS
;;; functions because they have been moved to the testing 
;;; file in order to make the output prettier.
;;; However, if the :VERBOSE? keyword argument is used, 
;;; the population and fitness cases will be printed and
;;; a report will be printed on every generation.


;;; FUNCTIONS in this file
;;;===========================================================
;;; RUN-GP-SYSTEM
;;  PRINT-INDIVIDUAL
;;; CREATE-INDI-PROG
;;; CREATE-POPULATION
;;; EVALUATE-POP-FITNESS
;;; ADJUSTED-FITNESS
;;; NORMALIZE-POPULATION-FITNESS
;;; SORT-POP-BY-FITNESS
;;; SELECT-FITNESS-PROP-INDIVIDUAL
;;; SELECT-INDIVIDUAL
;;; COUNT-CROSSOVER-POINTS
;;; TREE-DEPTH
;;; GET-SUBTREE
;;; VALIDATE-CROSSOVER
;;; CROSSOVER-AT-ANY-POINT
;;; CROSSOVER-AT-FUNCTION-POINTS
;;; GET-FUNCTION-SUBTREE
;;; MUTATE
;;; BREED-NEW-POP
;;; CLEAN-OUT-FITNESSES
;;; EXECUTE-GENEATIONS


;;; COMPILER-SETTINGS
;;;-----------------------------
;;; Only valid for allegro LISP implementations

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 3) (space 1) (debug 1))))


;;; Stuff to Load
;;;==========================================================

;; Randomizer functions
(load (compile-file "randomizer"))

;; Reporting functions
(load (compile-file "reporters"))

;; Protected Functions
(load (compile-file "protected-functions"))


;;;************************************************************
;;;   GLOBALS
;;;************************************************************

;;; *NUM-FITNESS-CASES*
;;;--------------------------
;;; The number of fitness cases
(defvar *num-fitness-cases* :unbound)

;;; *MAX-DEPTH-FOR-NEW-INDIS*
;;;----------------------------------------
(defvar *max-depth-for-new-indis* :unbound
 "The maximum depth for new individuals in the initial generation")

;;; *MAX-DEPTH-FOR-INDIVIDUALS-AFTER-CROSSOVER*
;;;--------------------------------------------
(defvar *max-depth-for-individuals-after-crossover* :unbound)

;;; *FITNESS-PROPORTIONATE-REPRODUCTION-FRACTION*
;;;----------------------------------------------
(defvar *fitness-proportionate-reproduction-fraction* :unbound)

;;; *CROSSOVER-AT-ANY-POINT-FRACTION*
;;;----------------------------------------------
(defvar *crossover-at-any-point-fraction* :unbound
"The fraction of the population that will experience
crossover at any point in the tree (including terminals)
during each generation")

;;; *CROSSOVER-AT-FUNCTION-POINT-FRACTION*
;;;------------------------------------------------
(defvar *crossover-at-function-point-fraction* :unbound
"The fraction of the population that will experience
crossover at a function (internal) point in the tree
during each generation.")

;;; *MAX-DEPTH-FOR-NEW-SUBTREES-IN-MUTATNS*
;;;--------------------------------------------------
(defvar *max-depth-for-new-subtrees-in-mutants* :unbound
"The maximum depth of new subtrees created by mutation")

;;; *METHOD-OF-SELECTION*
;;;-------------------------------------------------
(defvar *method-of-selection* :unbound)

;;; *METHOD-OF-GENERATION*
;;;--------------------------
;;; The method of generation for the initial population.
;;; Can be, :grow, :full, :ramped
(defvar *method-of-generation* :unbound
  "Can be :grow, :full, or :ramped")

;;; *SEED*
;;;-----------------------------------------
;;; For Park-Miller
(defvar *seed* :unbound)

;;; *BEST-OF-RUN-INDIVIDUAL*
;;;-----------------------------------------
(defvar *best-of-run-individual* :unbound)

;;; *GENERATION-OF-BEST-OF-RUN-INDIVIDUAL*
;;;-----------------------------------------
(defvar *generation-of-best-of-run-individual* :unbound)


;;; *GENERATION-0-UNIQUIFIER-TABLE*
;;;-----------------------------------------
(defvar *generation-0-uniquifier-table*
    (make-hash-table :test #'equal)
  "used to guarantee that all gen 0 individuals are unique.")


;;;++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Additional globals for debugging and testing
;;;++++++++++++++++++++++++++++++++++++++++++++++++++


;;; *POP-VEC*
;;;-----------------------------------------
;;; A vector that includes the population from each generation
;;; for debugging.
(defvar *pop-vec* :unbound)



;;;************************************************************
;;;   RUN-GP-SYSTEM
;;;************************************************************
;;;   INPUTS:   PROBLEM-FUNCTION
;;;             SEED, float
;;;             MAXIMUM-GENERATIONS, int
;;;             SIZE-OF-POP, int
;;;             :VERBOSE?, boolean
;;;             :LOG?, boolean
;;;             :LOG-FILE, string
;;;   OUTPUT:   None
;;;   SIDE-FX   Runs the system and prints reports

(defun run-gp-system (problem-function
		      seed		;usually 1.0
		      maximum-generations
		      size-of-pop
		      &key (verbose? nil) (log? nil)
			   log-file)
  
  ;; Check argument validity
  (assert (and (integerp maximum-generations)
	       (not (minusp maximum-generations)))
      (maximum-generations)
    "MAXIMUM-GENERATIONS must be a non-negative integer, ~
not ~S" maximum-generations)
  (assert (and (integerp size-of-pop)
	       (plusp size-of-pop))
      (size-of-pop)
    "Size-Of-Population must be a positive integer, ~
not ~S" size-of-pop)
  (assert (or (and (symbolp problem-function)
		   (fboundp problem-function))
	      (functionp problem-function))
      (problem-function)
    "Problem-Function must be a function.")
  (assert (numberp seed) (seed)
    "The randomizer seed must be a number")
  
  ;; Set *SEED*
  (setf *seed* (coerce seed 'double-float))
  
  ;; Initialize best-of-run variables
  (setf *generation-of-best-of-run-individual* 0)
  (setf *best-of-run-individual* nil)
  
  ;; Get the problem-specific functions
  (multiple-value-bind (function-set-creator
			terminal-set-creator
			fitness-cases-creator
			fitness-function
			parameter-definer
			termination-predicate)
      (funcall problem-function)
    ;; Get the f-set and a-map
    (multiple-value-bind (function-set argument-map)
	(funcall function-set-creator)

      ;; Set up parameters
      (funcall parameter-definer)
      
      ;; Hold: TERMINAL-SET, POPULATION, FITNESS-CASES,
      ;;       and NEW-PROGRAMS
      (let* ((terminal-set (funcall terminal-set-creator))
	     (population (create-population size-of-pop
					    function-set
					    argument-map
					    terminal-set))
	     (fitness-cases (funcall fitness-cases-creator))
	     ;; NEW-PROGRAMS: used for creating new pop
	     ;;  Created here to reduce consing
	     (new-programs (make-array size-of-pop)))

	;; Initialize *POP-VEC*
	(setf *pop-vec* (make-array maximum-generations))
	;; Save the population so it can be examined
	(setf (svref *pop-vec* 0) population)
	
	;; RUN THE SYSTEM!!!!
	(execute-generations population new-programs
			     fitness-cases maximum-generations
			     fitness-function termination-predicate
			     function-set argument-map
			     terminal-set
			     :verbose? verbose?
			     :log? log?
			     :log-file log-file)
	
	;; verbose mode
	(if verbose?
	      (values population fitness-cases))
	
	))))



;;;************************************************************
;;;   CREATE THE POPULATION
;;;************************************************************

;;;============================================================
;;;  INDIVIDUAL STRUCT
;;;============================================================
;;; The struct to represent an INDIVIDUAL in a population
;;; Fields:
;;;      PROGRAM -- The generated function
;;;      RAW-FITNESS, a vector containing the result of evaluating
;;;                   the program for each fitness case
;;;      The STANDARD-FITNESS-VECTOR stores the standardized 
;;;       fitness for each fitness case, so they can be examined
;;;       if desired.
;;;      STANDARDIZED-FITNESS, the sum of all of the elements
;;;                            of the STANDARD-FITNESS-VECTOR
;;;      HITS, the number of fitness cases where an individual
;;;            had a STANDARDIZED-FITNESS within 7% of the 
;;;            GOAL value.

(defstruct (individual (:print-function print-individual))
  program
  raw-fitness
  standard-fitness-vector
  standard-fit-percent-vector
  (standardized-fitness 0.0)
  (standardized-fitness-avg 0.0)
  (standardized-fitness-percent-avg 0.0)
  (adjusted-fitness 0.0)
  (normalized-fitness 0.0)
  (hits 0))


;;;============================================================
;;;  PRINT-INDIVIDUAL
;;;============================================================
;;; Prints the INDIVIDUAL struct

(defun print-individual(indi str depth)
  ;; Ignore the depth thing
  (declare (ignore depth))
  
  ;; clear a new line
  (format str "~%")
  
  ;; Print the indi
  (format str " Hits:            ~A~%"
	  (individual-hits indi))
  ;;(format str " Raw Fitness:     ~A~%" 
	  ;;(individual-raw-fitness indi))
  (format str " Std Fit Vector:  ~A~%" 
	  (individual-standard-fitness-vector indi))
  (format str " Std Fit (sum):   ~A~%" 
	  (individual-standardized-fitness indi))
  (format str " Std Fit Avg %:   ~A~%" 
	  (individual-standardized-fitness-percent-avg indi))
  (format str " Function Nodes:  ~A~%"
	  (count-crossover-points
	   (individual-program indi)))
  (format str " Function Depth:  ~A~%"
	  (tree-depth (individual-program indi)))
  (format str " Function:        ~A~%" 
	  (individual-program indi)))


;;;============================================================
;;;  CREATE-INDI-PROG
;;;============================================================
;;; Recursively create a function tree from the given function
;;; set and terminal set.
;;;
;;; For this implementation, the function will only
;;; create "full" trees..
;;;
;;;  INPUTS:  FUNCTION-SET, a set of functions
;;;           ARGUMENT-MAP, the number of argumnets each element
;;;            of the FUNCTION-SET takes
;;;           TERMINAL-SET, a set of terminals
;;;           ALLOWABLE-DEPTH, the remaining depth of the tree 
;;;            to be created.
;;;           TOP-NODE?, boolean, T if being called from the root node
;;;            and NIL otherwise. This lets the function be used to
;;;            create parts of trees for mutation.
;;;           FULL?, boolean, are we creating full trees or 
;;;            grown trees?
;;;           
;;;  OUTPUT:  A new function composed of elements of the given
;;;           sets

(defun create-indi-prog (function-set argument-map 
			 terminal-set allowable-depth top-node? full?)
  
  (labels
      ;; CHOOSE-TERMINAL
      ;;--------------------------
      ;; Returns a random element from TERM-SET
      ;;  INPUTS:  TERM-SET
      ;;  OUTPUT:  an element of TERM-SET
      ((choose-terminal (term-set)
	 (let ((rand (random-integer (length term-set))))
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
       ;;           FULL-P
       ;; OUTPUT:   Arguments for a function       
       (create-function-args (num-args func-set arg-map term-set al-dep full-p)
	 (if (= num-args 0)
	     ;; If there are no args left return NIL
	     nil
	   ;; Else: generate an ARG
	   (cons 
	    ;; Each ARG is a new branch
	    (create-indi-prog func-set arg-map term-set al-dep nil full-p)
	    ;; The next ARG is the next part of the CONS
	    (create-function-args (- num-args 1)
				  func-set arg-map term-set
				  al-dep full-p)))))
    
    ;; End of LABELS
    ;;-----------------      
    
    (cond
     ;; BC
     ((<= allowable-depth 0)
      ;; We're are the maximum depth so return a terminal
      (choose-terminal terminal-set))
     
     ;; We're at the top node, or are making a full tree
     ;; so pick a function
     ((or full? top-node?)
      (let* ((rand (random-integer (length function-set)))
	     (fxn (elt function-set rand))
	     (num-as (elt argument-map rand)))
	;; CONS FXN to ARGS
	(cons fxn
	      ;; ARGS
	      (create-function-args num-as 
				    function-set
				    argument-map 
				    terminal-set 
				    (- allowable-depth 1)
				    full?))))
     (t
      ;; Choose something from the combined sets
      (let ((rand (random-integer (+ (length terminal-set)
				     (length function-set)))))
	(if (< rand (length function-set))
	    ;; If a function is chosen pick it out
	    (let* ((fxn (elt function-set rand))
		   (num-as (elt argument-map rand)))
	      ;; CONS FXN to ARGS
	      (cons fxn
		    ;; ARGS
		    (create-function-args num-as 
					  function-set
					  argument-map 
					  terminal-set 
					  (- allowable-depth 1)
					  full?)))
	  ;; Else: We chose a terminal
	  (choose-terminal terminal-set)))))))



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

(defun create-population 
    (pop-size function-set argument-map terminal-set)
  "Creates the population.  The population is an array of size
 POP-SIZE."
  
  ;; Initialize the population vector
  (let ((population (make-array pop-size))
	(minimum-depth-of-trees 1)
	(attempts-at-this-indi 0)
	(full-cycle-p nil))
    
    ;; Loop through the pop vector and setf the PROGS
   (do ((i 0))
       ;; End test form
       ((>= i pop-size))
     ;; Statements
     
     ;; Flip FULL-CYCLE-P to use ramped-half-and-half generation
     (when (zerop (mod i (max 1 (- *max-depth-for-new-indis*
				   minimum-depth-of-trees))))
       (setf full-cycle-p (not full-cycle-p)))
     
     ;; Create the new program
     (let ((new-prog (create-indi-prog 
		      function-set 
		      argument-map 
		      terminal-set 
		      ;; Allowable depth
		      (ecase *method-of-generation*
			((:full :grow)
			 *max-depth-for-new-indis*)
			(:ramped
			 (+ minimum-depth-of-trees
			    (mod i
				 (- *max-depth-for-new-indis*
				    minimum-depth-of-trees)))))
		      ;; top-node?
		      t
		      ;; full?
		      (ecase *method-of-generation*
			(:full t)
			(:grow nil)
			(:ramped full-cycle-p)))))
       
       ;; Check if NEW-PROG is unique
       ;; If it is, store it.  If not, try again.
       (cond
	;; Case1: It's unique
	((not (gethash new-prog *generation-0-uniquifier-table*))
	 ;; Create the new INDI
	 (setf (svref population i)
	     (make-individual
	      :program new-prog))
	 ;; Set the hash to T
	 (setf (gethash new-prog *generation-0-uniquifier-table*)
	   t)
	 ;; Attempts
	 (setf attempts-at-this-indi 0)
	 (incf i))
	
	;; Case2: Attempts > 20
	((> attempts-at-this-indi 20)
	 ;; If we get here this depth is probably full so we
	 ;; need to innf the depth
	 (incf minimum-depth-of-trees)
	 ;; Also increase *MAX-DEPTH-FOR-NEW-INDIS*
	 (setf *max-depth-for-new-indis*
	   (max *max-depth-for-new-indis* 
		minimum-depth-of-trees)))
	;; Final Case: it's not unique, try again
	(t
	 (incf attempts-at-this-indi)))))
   
   ;; Clean out the hash table
   (clrhash *generation-0-uniquifier-table*)
   
   ;; Return the new POPULATION
   population))



;;;************************************************************
;;;   FITNESS
;;;************************************************************

;;;============================================================
;;;  EVALUATE-POP-FITNESS
;;;============================================================
;;; Evaluates the fitness of every individual in a population.
;;;
;;;  INPUTS:  POPULATION, a vector of INDIVUDUALS
;;;           FITNESS-CASES, for this version there is only one
;;;           FITNESS-FXN, just EVAL in this version.
;;;  OUTPUT:  The POPULATION vector with fitnesses

(defun evaluate-pop-fitness (population fitness-cases fitness-fxn)
  "Loops through the population vector and evaluates the fitness 
   of each and the HITS."
  
  (dotimes (index (length population))
    ;; Hold the current individual
    (let ((indi (svref population index)))
      (multiple-value-bind (raw-fitness 
			    standard-fitness-vector
			    standard-fit-percent-vector
			    standardized-fitness
			    average-standardized-fitness
			    average-std-fitness-percent
			    hits)
	  (funcall fitness-fxn
		   (individual-program indi)
		   fitness-cases)
	;; Record fitnesses and hits for this INDI
	(setf (individual-raw-fitness indi) raw-fitness)
	(setf (individual-standard-fitness-vector indi) 
	  standard-fitness-vector)
	(setf (individual-standard-fit-percent-vector indi)
	  standard-fit-percent-vector)
	(setf (individual-standardized-fitness indi) 
	  standardized-fitness)
	(setf (individual-standardized-fitness-avg indi)
	  average-standardized-fitness)
	(setf (individual-standardized-fitness-percent-avg indi)
	  average-std-fitness-percent)
	(setf (individual-hits indi) hits)))))


;;;  ADJUSTED-FITNESS
;;;============================================================
;;;   INPUTS:   STANDARDIZED-FITNESS, int
;;;   OUTPUT:   ADJUSTED-FITNESS, int

(defun adjusted-fitness (standardized-fitness)
  "Calculates the adjusted fitness of an individual (program)."
  
  (/ 1.0 (+ 1.0 standardized-fitness)))


;;;============================================================
;;;  NORMALIZE-POPULATION-FITNESS
;;;============================================================
;;;   INPUTS:   POPULATION, vector of INDIVIDUALS
;;;   OUTPUT:   The POPULATION with normalized, and adjusted 
;;;             fitnesses evaluated

(defun normalize-population-fitness (population)
  "Calculates and sets the normalized and adjusted fitnesses
 of all of the individuals in a population."
  
  (let ((sum-of-adj-fitnesses 0.0))
    ;; Loop through the population and sum their adj-fitnesses
    (dotimes (i (length population))
      (let ((indi (svref population i)))
	;; Set ajdusted fitness
	(setf (individual-adjusted-fitness indi)
	  (adjusted-fitness 
	   (individual-standardized-fitness-percent-avg indi)))
	;; Incf sum of adj fitnesses
	(incf sum-of-adj-fitnesses 
	      (individual-adjusted-fitness indi))))
    
    ;; set the normalized fitness of each individual
    (dotimes (j (length population) population)
      (let ((indi (svref population j)))
	(setf (individual-normalized-fitness indi)
	  (/ (individual-adjusted-fitness indi)
	     sum-of-adj-fitnesses))))))


;;;============================================================
;;;  SORT-POP-BY-FITNESS
;;;============================================================
;;; Destructively sorts the population according to normalized
;;; fitness
;;;
;;;  INPUTS:  POPULATION, a vector of INDIVUDUALS
;;;  OUTPUT:  The POPULATION vector sorted by (normalized) fitness

(defun sort-pop-by-fitness (population)
  "Sorts the population by (normalized) fitness. Destructively."
  
  (sort population #'> :key #'individual-normalized-fitness))



;;;************************************************************
;;;   BREEDING
;;;************************************************************

;;;============================================================
;;;  SELECT-FITNESS-PROP-INDIVIDUAL
;;;============================================================
;;; Selects an individual in the given population with 
;;; a normalized fitness greater than the given value.
;;;
;;;  INPUTS:   FITNESS-LEVEL, int
;;;            POPULATION        
;;;  OUTPUT:   An INDIVIDUAL-PROGRAM

(defun select-fitness-prop-individual (fitness-level population)
  "Finds an individual in the specified population whose
   normalized fitness is greater than the specified value.
   All we need to do is count along the population from the
   beginning adding up the fitness until we get past the
   specified point."
  (let* ((pop-size (length population))
	 (sum-of-fitness 0.0))
    
    (let ((index-of-selected-individual
           (do ((index 0 (+ index 1)))
	       ;; Exit condition
               ((or (>= index pop-size)
                    (>= sum-of-fitness fitness-level))
                (if (>= index pop-size)
                    (- (length population) 1)
		  (- index 1)))
	     ;; Body.  Sum up the fitness values.
	     (incf sum-of-fitness
		   (individual-normalized-fitness
                    (svref population index))))))
      ;; Return
      (individual-program
       (svref population index-of-selected-individual)))))


;;;============================================================
;;;  SELECT-INDIVIDUAL
;;;============================================================
;;; Selects an individual in the given population.
;;;
;;;  INPUTS:   POPULATION        
;;;  OUTPUT:   An INDIVIDUAL PROGRAM

(defun select-individual (population)
  "Selects an individual from the given population (according to
  the defined selection method)."
  
  (select-fitness-prop-individual (random-floating-point-number 1.0) 
				  population))


;;;============================================================
;;;  COUNT-CROSSOVER-POINTS
;;;============================================================
;;; Counts all of the crossover points in a tree (program). 
;;; For our purposes, each node in our tree (either a function
;;; or a terminal) is considered a crossover point.  The 
;;; the numbering is done depth-first from left to right.
;;;
;;;  INPUTS:  PROGRAM, a LISP S-expression
;;;  OUTPUT:  An int givng the number of corssover points 
;;;            (or nodes).

(defun count-crossover-points (program)
  "Counts all of the crossover points in a tree (program)."
  
  (if (consp program)
      (+ 1 
	 (reduce #'+ 
		 (mapcar #'count-crossover-points 
			 (rest program))))
    ;; else
    1))


;;;============================================================
;;;  TREE-DEPTH
;;;============================================================
;;; Finds the depth of the deepest branch of the given tree
;;; (program).
;;;
;;;  INPUTS:  TREE, a LISP S-expression
;;;  OUTPUT:  An int givng the depth of the deepest branch

(defun tree-depth (tree)
  "Returns the depth of the deepest/longest branch of the 
  given tree (program)."
  
  (if (consp tree)
      (+ 1 
	 (if (rest tree)
	       (apply #'max 
		      (mapcar #'tree-depth (rest tree)))
	   ;; Else
	   0))
    ;; Else
    1))


;;;============================================================
;;;  GET-SUBTREE
;;;============================================================
;;; Yanks a subtree out from a given tree at the index 
;;; specified.
;;;
;;;  INPUTS:   TREE, a LISP S-expression
;;;            POINTER, a pointer to a tree/subtree
;;;            INDEX, int, 
;;;  OUTPUTS:  a pointer to a tree
;;;            TREE
;;;            int

(defun get-subtree (tree pointer index)
 "Given a tree or subtree, a pointer to that tree/subtree, and 
 an index, return the component subtree numbered by that index.
 Indecies are assigned in a depth-first manner, left to right."
 
 (if (= index 0)
     ;; If the index points to the root
     (values pointer (copy-tree tree) index)
  ;; Else
  (if (consp tree)
      ;; If TREE is a list wlak through the tree looking for 
      ;; INDEX
      (do* ((tail (rest tree) (rest tail))
            (argument (first tail) (first tail)))
           ;; End test form
           ((not tail) (values nil nil index))
           ;; Evaluate this
           (multiple-value-bind
               (new-pointer new-tree new-index)
               (get-subtree argument tail (- index 1))
            (if (= new-index 0)
                (return 
                 (values new-pointer new-tree new-index))
                (setf index new-index))))
      ;; Else
      (values nil nil index))))
              

;;;============================================================
;;;  VALIDATE-CROSSOVER
;;;============================================================
;;; Checks if a child from crossover exceeds the max depth and 
;;; returns the parent if it does
;;; 
;;;   INPUTS:   MALE, a tree struct
;;;             NEW-MALE, list
;;;             FEMALE, tree struct
;;;             NEW-FEMALE, list
;;;   OUTPUTS:  

(defun validate-crossover (male new-male female new-female)
 "Given the old and new males and females from a crossover, 
 check if the child exceedes the maximum allowed depth."
 
 (let ((new-male-depth (tree-depth (first new-male)))
       (new-female-depth (tree-depth (first new-female))))
 
  (values
   ;; Check males
   (if (or (= 1 new-male-depth)
           (> new-male-depth 
	      *max-depth-for-individuals-after-crossover*))
       ;; If NEW-MALE-DEPTH exceeds *MAX-DEPTH-for...* return MALE
       male
    ;; Else
    (first new-male))
   ;; Check Females
   (if (or (= 1 new-female-depth)
           (> new-female-depth 
	      *max-depth-for-individuals-after-crossover*))
        female
    ;; Else
    (first new-female))))) 


;;;============================================================
;;;  CROSSOVER-AT-ANY-POINT
;;;============================================================
;;; Takes in two trees and performs a crossover operation at
;;; any point in the trees.
;;;   INPUTS:   MALE, a tree struct
;;;             FEMALE, a tree struct
;;;   OUTPUT:   Two new trees 

(defun crossover-at-any-point (male female)
 "Performs a crossover operation on the given trees."
 
 ;; Pick the crossover points and copy the trees
 (let*
       ((male-point (random-integer (count-crossover-points male)))
        (female-point (random-integer (count-crossover-points female)))
        (new-male (list (copy-tree male)))
        (new-female (list (copy-tree female))))
       
      ;; Get pointers to the subtrees indexed by MALE-POINT and
      ;; FEMALE-POINT
      (multiple-value-bind (male-subtree-pointer male-fragment)
            (get-subtree (first new-male) new-male male-point)
            ;; And another one
            (multiple-value-bind (female-subtree-pointer female-fragment)
                  (get-subtree (first new-female) new-female female-point)
             ;; Swap in the  subtree
             (setf (first male-subtree-pointer) female-fragment)
             (setf (first female-subtree-pointer) male-fragment)))
      ;; Make sure the new individuals aren't too big
      (validate-crossover male new-male female new-female)))


;;; CROSSOVER RESTRICTED TO FUNCTION POINTS
;;;------------------------------------------------------------
;;; In ofder to prevent an inordinate amount of small crossovers,
;;; the majority of crossover is restricted to internal (function)
;;; points and these functions are used for this.

;;;============================================================
;;;  CROSSOVER-AT-FUNCTION-POINTS
;;;============================================================
;;; Takes in two trees and performs a crossover operation at
;;; an internal point in the trees.
;;;   INPUTS:   MALE, a tree (function)
;;;             FEMALE, a tree 
;;;   OUTPUT:   Two new trees 

(defun crossover-at-function-points (male female)
  "Performs crossover on the two programs at a function
[internal] point in the trees."
  ;; Pick the function (internal) points in the respective trees 
  ;; on which to perform the crossover.
  (let ((male-point
	 (random-integer (count-function-points male)))
	(female-point
	 (random-integer (count-function-points female))))
    ;; Copy the trees because we destructively modify the new
    ;; individuals to do the crossover and Reselection is
    ;; allowed in the original population. Not copying would
    ;; cause the individuals in the old population to
    ;; be modified.
    (let ((new-male (list (copy-tree male)))
	  (new-female (list (copy-tree female))))
      ;; Get the pointers to the subtrees indexed by male-point
      ;; and female-point
      (multiple-value-bind (male-subtree-pointer male-fragment)
	  (get-function-subtree
	   (first new-male) new-male male-point)
	(multiple-value-bind
	    (female-subtree-pointer female-fragment)
	    (get-function-subtree
	     (first new-female) new-female female-point)
	  ;; Modify the new individuals by smashing in
	  ;; the (copied) subtree from the old individual.
	  (setf (first male-subtree-pointer) female-fragment)
	  (setf (first female-subtree-pointer) male-fragment)))
      ;; Make sure that the new individuals aren't too big.
      (validate-crossover male new-male female new-female))))


;;;============================================================
;;;  COUNT-FUNCTION-POINTS
;;;============================================================
;;;   INPUTS:   PROGRAM, a tree
;;;   OUTPUT:   int

(defun count-function-points (program)
  "Counts the number of function (internal) points
in the program."
  (if (consp program)
      (+ 1 (reduce #'+ (mapcar #'count-function-points
			       (rest program))))
    0))


;;;============================================================
;;;  GET-FUNCTION-SUBTREE
;;;============================================================
;;;   INPUTS:   TREE, a function
;;;             POINTER-TO-TREE
;;;             INDEX, int
;;;   OUTPUT:   SUBTREE

(defun get-function-subtree (tree pointer-to-tree index)
  "Given a tree or subtree, a pointer to that tree/subtree and
an index return the component subtree that is labeled with
an internal point that is numbered by Index. We number left
to right, depth first."
  (if (= index 0)
      (values pointer-to-tree (copy-tree tree) index)
    (if (consp tree)
	(do* ((tail (rest tree) (rest tail))
	      (argument (first tail) (first tail)))
	    ((not tail) (values nil nil index))
	  (multiple-value-bind
	      (new-pointer new-tree new-index)
	      (if (consp argument)
		  (get-function-subtree
		   argument tail (- index 1))
		(values nil nil index))
	    (if (= new-index 0)
		(return
		  (values new-pointer new-tree new-index))
	      (setf index new-index))))
      (values nil nil index))))


;;; MUTATION
;;;------------------------------------------------------------
;;; A function to perform mutation, just in case we need it.

;;;============================================================
;;;  MUTATE
;;;============================================================
;;;   INPUTS:   PROGRAM
;;;             FUNCTION-SET
;;;             ARGUMENT-MAP
;;;             TERMINAL-SET
;;;   OUTPUT:   A new program
;;;             and a new subtree

(defun mutate (program function-set argument-map terminal-set)
  "Randomly selects a node in the given tree (function) and 
  replaces it with a new subtree generated in the same way 
  as the initial population."
  
  ;; Pick the mutation point and create the new subtree
  (let* ((mutation-point (random-integer 
			  (count-crossover-points program)))
         (new-subtree (create-indi-prog 
		       function-set 
		       argument-map 
		       terminal-set 
		       *max-depth-for-new-subtrees-in-mutants* 
		       t 
		       nil))
         (new-program (list (copy-tree program))))
    
    (multiple-value-bind (subtree-pointer fragment)
	;; Get the pointer to the mutation point
	(get-subtree (first new-program) 
		     new-program mutation-point)
      ;; Don't care about the old subtree
      (declare (ignore fragment))
      ;; Stick in the new subtree
      (setf (first subtree-pointer) new-subtree))
    ;; Return values
    (values (first new-program) new-subtree)))


;;;============================================================
;;;  BREED-NEW-POP
;;;============================================================
;;; Creates the new population!!!
;;;
;;;   INPUTS:   POPULATION, vector of INDIVIDUALS
;;;             NEW-PROGS,
;;;             FUNCTION-SET
;;;             ARGUMENT-MAP
;;;             TERMINAL-SET
;;;             :debug?  boolean
;;;   OUTPUT:   The new population

(defun breed-new-pop
    (population new-progs function-set argument-map terminal-set)
  "Executes the actual breeding of the new population.  Loops through
 the population performing crossover operations until the specified
 fraction is reached."
  
  (let ((pop-size (length population)))
    
    (do ((index 0)
	 (fraction 0 (/ index pop-size)))
	;; End Condition
	((>= index pop-size))
      ;; Block to run
      (let ((indi-1 (select-individual population)))
	
	(cond
	 ;; Case1: Crossover
	 ((and (< index (- pop-size 1))
	       (< fraction 
		  (+ *crossover-at-function-point-fraction*
		     *crossover-at-any-point-fraction*)))
	  (multiple-value-bind (new-male new-female)
	      (funcall
	       (if (< fraction
		      *crossover-at-function-point-fraction*)
		   'crossover-at-function-points
		 ;; Else
		 'crossover-at-any-point)
	       ;; Then use these aguments for the selected 
	       ;;crossover function
	       indi-1
	       (select-individual population))
	    (setf (svref new-progs index) new-male)
	    (setf (svref new-progs (+ 1 index)) new-female))
	  (incf index 2))
	 ;; Case 2: Fitness proportionate reproduction
	 ((< fraction 
	     (+ *fitness-proportionate-reproduction-fraction*
		*crossover-at-function-point-fraction*
		*crossover-at-any-point-fraction*))
	  (setf (svref new-progs index) indi-1)
	  (incf index 1))
	 ;; Final case: Use Mutation for any spots left over
	 (t
	  (setf (svref new-progs index)
	    (mutate indi-1 function-set argument-map terminal-set))
	  (incf index 1)))))
    
    ;; Now walk through the population and set the new programs
    (dotimes (j pop-size)
      (setf (individual-program (svref population j))
	(svref new-progs j)))))



;;;************************************************************
;;;   RUN  THAT SHIT!!!!!!!
;;;************************************************************

;;;============================================================
;;;  CLEAN-OUT-FITNESSES
;;;============================================================
;;; Cleans out old fitness measure, just in case.
;;;
;;;   INPUTS:   POPULATION, vector of INDIVIDUALS
;;;   OUTPUT:   NONE
;;;   SIDE-FX:  Cleans out fitnesses of POPULATION

(defun clean-out-fitnesses (population)
  (dotimes (i (length population))
    (let ((indi (svref population i)))
      (setf (individual-raw-fitness indi) 0)
      (setf (individual-standardized-fitness indi) 0)
      (setf (individual-adjusted-fitness indi) 0)
      (setf (individual-normalized-fitness indi) 0)
      (setf (individual-hits indi) 0))))


;;;============================================================
;;;  EXECUTE-GENERATIONS
;;;============================================================
;;; RUNS THAT SHIT!!!
;;;
;;;   INPUTS:   POPULATION, vector of INDIVIDUALS
;;;             NEW-PROGS
;;;             FITNESS-CASES
;;;             MAXIMUM-GENERATION
;;;             FITNESS-FUNCTION
;;;             TERMINATION-PREDICATE
;;;             FUNCTION-SET
;;;             ARGUMENT-MAP
;;;             TERMINAL-SET
;;;             :VERBOSE?
;;;             :LOG?
;;;   OUTPUT:   Victory

(defun execute-generations (population new-programs fitness-cases
			    max-generations fitness-function 
			    termination-predicate function-set
			    argument-map terminal-set
			    &key (verbose? nil) (log? nil) log-file)
  "Runs that shit!!!!  Loops intil the termination predicate 
  says to stop or MAX-GENERATIONS is reached."
  
  (do ((current-generation 0 (+ 1 current-generation)))
      ;; End predicate: Loop incrementing current generation until 
      ;; termination predicate succedes.
      ((let ((best-of-generation (svref population 0)))
	 (funcall termination-predicate 
                  current-generation 
		  max-generations
                  (individual-standardized-fitness-percent-avg
		   best-of-generation)
		  (individual-hits best-of-generation))))
    
    (when (> current-generation 0)
      ;; Breed the new pop for this (non-zero) generation
      (breed-new-pop population new-programs function-set
		     argument-map terminal-set)
      (setf (svref *pop-vec* current-generation) population))
    
    ;; Clean out old fitnesses
    (clean-out-fitnesses population)
    
    ;; Evaluate fitness
    (evaluate-pop-fitness population fitness-cases fitness-function)     
    ;; Normalize fitness
    (normalize-population-fitness population)     
    ;; Sort the population
    (sort-pop-by-fitness population)
    
    ;; Keep track of best of run individual
    (let ((best-of-generation (svref population 0)))
      (when (or (not *best-of-run-individual*)
		(> (individual-standardized-fitness *best-of-run-individual*)
		   (individual-standardized-fitness best-of-generation)))
	(setf *best-of-run-individual* (copy-individual best-of-generation))
	(setf *generation-of-best-of-run-individual* current-generation)))
    
    ;; Print statistics
    (cond 
     ;; VERBOSE?
     (verbose?
      (report-on-generation current-generation population))
     ;; LOG?
     (log?
      (with-open-file (stream log-file :direction :output
		       :if-exists :append :if-does-not-exist :create)
	;; Get average fitness
	(let ((size-of-population (length population))
	      (sum 0.0)
	      (*print-pretty* t))
	  ;; Add up all of the standardized fitnesses to get average
	  (dotimes (index size-of-population)
	    (incf sum (individual-standardized-fitness-percent-avg
		       (aref population index))))
	  ;; Print nice summary
	  (format stream "~%Generation ~D: Average standardized-fitness ~
= ~S. ~%~
The best individual program of the population ~
had a ~%standardized fitness measure of ~D ~
and ~D hit~P. ~%It was: ~%~S~%"
		  current-generation (/ sum (length population))
		  (individual-standardized-fitness-percent-avg 
		   *best-of-run-individual*)
		  (individual-hits *best-of-run-individual*)
		  (individual-hits *best-of-run-individual*)
		  (individual-program *best-of-run-individual*)))))
     ;; There's not really another case here 
     (t
      nil))
         
      ))
 






;;; EOF :P