;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  kernel-1.lisp
;;; ========================================

;;;   FUNCTIONS
;;;===========================================================
;;; CREATE-INDI-PROG
;;; CREATE-POPULATION
;;; EVALUATE-POP-FITNESS
;;;   STANDARDIZED-FITNESS
;;;   NORMALIZED-FITNESS
;;; SORT-POP-BY-FITNESS
;;; SELECT-FITNESS-PROP-INDIVIDUAL
;;; SELECT-INDIVIDUAL
;;; COUNT-CROSSOVER-POINTS
;;; TREE-DEPTH
;;; GET-SUBTREE
;;; VALIDATE-CROSSOVER
;;; CROSSOVER


;;;   TO-DO!!!
;;;************************************************************
;;;  2. "Ramp-up" for indi-progs/create-pop
;;;  4. FITNESS-CASES
;;;  6. "Uniquify" initial generation
;;;  



;;;   DONE
;;;**************************************************
;;;  -  BREED-NEW-POPULATION
;;;  9. PREVENT DIVISION BY ZERO!!!!!
;;;  5. FITNESS
;;;     a. Standardized: expresses as error (s.t. smaller is better)
;;;     b. Adjusted: between 1 and 0
;;;     c. Normalized: btw 1 and 0 AND sums to 1
;;;     d. b and c are also larger for better individuals
;;;  7. Breed new POP
;;;    a. Select individual
;;;    b. COPY-TREE <--- IT'S BUILT IN HAHAHAHAHAHAHAAAA!!!!!!!!
;;;    c. GET-SUBTREE
;;;    d. COUNT-CROSSOVER-POINTS
;;;  8. Mutation?
;;;  1. "Grown" trees for indi-progs
;;;  3. Top-node-p for create-indi-prog





;;;   NOTES
;;;**************************************************

;; 1. Setting the max depth for trees after crossover operations
;;      Current:  Invalid child returns adult
;;      Other Options:
;;        - Truncate child
;;        - Simplify?
;;
;; 2. Protected Functions
;;      Should dividing by zero really return 1?? ...







;;;************************************************************
;;;   GLOBALS
;;;************************************************************

;;; *GOAL*
;;;----------------------
;;; For this early vearion of the kernel, the test case is 
;;; creating a function with the largest numerical output
;;; possible.  With a depth of 3 and a max value in the 
;;; terminal set of 5, that value is 5^8 or 309625.  This
;;; is the value that will be used to calculate the
;;; standardized fitness.
(defvar *goal* 390625)

;;; *POPULATION*
;;;--------------------------------------
(defvar *population* nil)

;;; POP-SIZE
;;;----------------------------------------
;;(defvar *pop-size* 100)

;;; *HEIGHT*
;;;----------------------------------------
(defvar *height* 3
 "The maximum depth for new individuals in the initial generation")


;;; *MAX-DEPTH*
;;;----------------------------------------
(defvar *max-depth* 7
  "The maximum depth for any tree...ever.")


;;; *KEEPERS*
;;;----------------------------------------
;;; fraction of the population to keep between generations
(defvar *keepers* 0.05)


;;; *SEED*
;;;-----------------------------------------
;;; For Park-Miller
(defvar *seed* :unbound)

;;; *BEST-OF-RUN-INDIVIDUAL*
;;;-----------------------------------------
(defvar *best-of-run-individual* :unbound)

(defvar *generation-of-best-of-run-individual* :unbound)



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



;;;************************************************************
;;;   CREATE THE POPULATION
;;;************************************************************

;;;============================================================
;;;  INDIVIDUAL-STRUCT
;;;============================================================
;;; The struct to represent an INDIVIDUAL in a population
;;; Fields:
;;;      PROGRAM -- The generated function
;;;      FITNESSES...the fitnesses

(defstruct individual
  program
  (raw-fitness 0)
  (standardized-fitness 0)
  (adjusted-fitness 0)
  (normalized-fitness 0))


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
				    (- allowable-depth 1)
                full?))))
     (t
      ;; Choose something from the combined sets
      (let ((rand (random (+ (length terminal-set)
                             (length function-set)))))
         (if (< rand (length function-set))
             ;; If a function is chosen pick it out
             (let* ((fxn (elt rand function-set))
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

(defun create-population (pop-size function-set argument-map terminal-set)
 "Creates the population.  The population is an array of size
 POP-SIZE."
 
 ;; Initialize the population vector
 (let ((population (make-array pop-size)))
   ;; Loop through the pop vector and setf the PROGS
   (dotimes (i pop-size population)
     (let ((prog (create-indi-prog function-set argument-map
				   terminal-set *height* t nil)))
       (setf (svref population i)
	 (make-individual
	  :program prog))))
   
   ;;(setf *population* population)
   ))



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

(defun evaluate-pop-fitness (population fitness-fxn &optional (fitness-cases 1))
  "Loops through the population vector and evaluates the fitness 
   of each"
  
  (dotimes (i (length population) population)
    (let* ((indi (svref population i))
	   (indi-prog (individual-program indi))
	   (indi-raw (funcall fitness-fxn indi-prog))
	   (indi-stand (standardized-fitness indi-raw))
	   (indi-adj (adjusted-fitness indi-stand)))
      
      ;; Set the fitness of each INDI
      (setf (individual-raw-fitness indi) indi-raw)
      (setf (individual-standardized-fitness indi) indi-stand)
      (setf (individual-adjusted-fitness indi) indi-adj))))


;;;  STANDARDIZED-FITNESS
;;;============================================================

(defun standardized-fitness (raw-fitness)  ;; & optional goal
  "Calculates the standardized fitness of an individual (program)."
  ;; The standardized fitness, is really just the raw fitness 
  ;; expressed in a way s.t. a smaller value is better.  In this 
  ;; case it's just the error.
  (abs (- *goal* raw-fitness)))


;;;  ADJUSTED-FITNESS
;;;============================================================

(defun adjusted-fitness (standardized-fitness)
  "Calculates the adjusted fitness of an individual (program)."
  
  (/ 1.0 (+ 1.0 standardized-fitness)))


;;;============================================================
;;;  NORMALIZE-POPULATION-FITNESS
;;;============================================================

(defun normalize-population-fitness (population)
  "Calculates the normalized fitness of all of the individuals
  in a population."
  
  (let ((sum-of-adj-fitness 0.0))
    ;; Loop through the population and sum their adj-fitnesses
    (dotimes (i (length population))
      (let ((indi (svref population i)))
	(incf sum-of-adj-fitness 
	      (individual-adjusted-fitness indi))))
    
    ;; set the normalized fitness of each individual
    (dotimes (j (length population) population)
      (let ((indi (svref population j)))
	(setf (individual-normalized-fitness indi)
	  (/ (individual-adjusted-fitness indi)
	     sum-of-adj-fitness))))))


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
;;;  OUTPUT:   An INDIVIDUAL

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
                    (aref population index))))))
     (individual-program
       (aref population index-of-selected-individual)))))


;;;============================================================
;;;  SELECT-INDIVIDUAL
;;;============================================================
;;; Selects an individual in the given population.
;;;
;;;  INPUTS:   POPULATION        
;;;  OUTPUT:   An INDIVIDUAL

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
		      (mapcar #'max-depth-of-tree (rest tree)))
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
           (> new-male-depth *max-depth*))
       ;; If MALE-DEPTH is 1 of exceeds *MAX-DEPTH* return MALE
       male
    ;; Else
    (first new-male))
   ;; Check Females
   (if (or (= 1 new-female-depth)
           (> new-female-depth *max-depth*))
        female
    ;; Else
    (first new-female))))) 



;;;============================================================
;;;  CROSSOVER
;;;============================================================
;;; Takes in two trees and performs a crossover operation at
;;; any point in the trees.
;;;   INPUTS:   MALE, a tree struct
;;;             FEMALE, a tree struct
;;;   OUTPUT:   Two new trees 

(defun crossover (male female)
 "Performs a crossover operation on the given trees."
 
 ;; Pick the crossover points and copy the trees
 (let*
       ((male-point (random (count-crossover-points male)))
        (female-point (random (count-crossover-points female)))
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


;;;============================================================
;;;  MUTATE
;;;============================================================
;;; I'm only defining this function in order to deal with an 
;;; odd-man-out scenario when breeding... :(
;;;
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
  (let* ((mutation-point (random-integer (count-crossover-points program)))
         (new-subtree (create-indi-prog function-set argument-map 
                                       terminal-set *max-depth* t nil))
         (new-program (list (copy-tree program))))
    (multiple-value-bind (subtree-pointer fragment)
     ;; Get the pointer to the mutation point
     (get-subtree (first new-program) new-program mutation-point)
     ;; Don't care about the old subtree
     (declare (ignore fragment))
     ;; Stick in the new subtree
     (setf (first subtree-pointer) new-subtree))
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
;;;   OUTPUT:   The new population

(defun breed-new-pop
       (population new-progs function-set argument-map terminal-set)
 "Executes the actual breeding of the new population.  Loops through
 the population performing crossover operations until the specified
 fraction is reached."
 
 (let ((pop-size (length population)))
  
  (do ((index 0)
       (fraction 0 (/ index population)))
      ;; End Condition
      ((>= index pop-size))
   ;; Block to run
   (let ((indi-1 (select-individual population)))
         ;;(indi-2 (select-individual population)))
    
    (cond
     ;; Case1: KEEPERS!
     ;; We want to keep the top 5% of the population
     ((<= fraction *keepers*)
      (setf (svref new-progs index) 
            (individual-program (svref population index)))
      (incf index 1))
     ;; Case2: INDEX is less than POP-SIZE and we're still in the top 95% of
     ;;     the population
     ((and (< index (- pop-size 1))
           (< fraction (- 1 *keepers*)))
       (multiple-value-bind (new-male new-female)
        (crossover indi-1
                   (select-individual population))
        ;; Set values
        (setf (svref new-progs index) new-male)
        (setf (svref new-progs (+ 1 index)) new-female))
       (incf index 2))
     ;; Otherwise, MUTATE
     (t
      (setf (svref new-progs index)
            (mutate indi-1 function-set argument-map terminal-set))
      (incf index 1)))))
    
    ;; Now walk through the population and set the new programs
    (dotimes (j pop-size)
     (setf (individual-program (svref population j))
           (svref new-progs j)))))
       


;;;************************************************************
;;;   RANDOM NUMBER GENERATORS
;;;************************************************************
;;; These functions were not directly relevant to the project
;;; and reimplementing in some new way would have been more 
;;; difficult than I had time for.  However, I could not overlook
;;; the value of having truely good random seed data for the 
;;; algorithm so I copied these functions directly (I actually 
;;; did make one or two small changes) from Koza's book
;;; (pp. 754-755).

(defun park-miller-randomizer ()
 "The Park-Miller multiplicative congruential randomizer
  (CACM, October 88, Page 1195). Creates pseudo random floating
  point numbers in the range 0.0 < x <= 1.0. Record the seed
  value, called *seed*, to make your runs reproducible.
  NOTE: 16807 is (expt 7 5) and 2147483647 is (- (except 2 31) 1)"
  
  #+Lucid (unless (typep *seed* 'integer)
            (setq *seed* (round *seed*)))
   (assert (not (zerop *seed*)) () "*seed* cannot be zero.")
   (let* ((multiplier #+Lucid 16807 #-Lucid 16807.0d0)
         (modulus #+Lucid 2147483647 #-Lucid 2147483647.0d0)
         (temp (* multiplier *seed*)))
   
      (setf *seed* (mod temp modulus)))
      (#+lucid float #-lucid progn (/ *seed* modulus)))


(defun random-floating-point-number (n)
 "Returns a pseudo random floating-point number
  in range 0.0 <= number < n"
  
  (let ((random-number (park-miller-randomizer)))
   ;; We subtract the randomly generated number from 1.0
   ;; before scaling so that we end up in the range
   ;; 0.0 <= x < 1.0, not 0.0 < x <= 1.0
   (* n (- 1.0d0 random-number))))

(defun random-integer (n)
 "Returns a pseudo-random integer in the range 0 ---> n-l."
 (let ((random-number (random-floating-point-number 1.0)))
 (floor (* n random-number))))






;;;************************************************************
;;;   RUN  THAT SHIT!!!!!!!
;;;************************************************************

;;;============================================================
;;;  EXECUTE-GENERATIONS
;;;============================================================
;;; RUNS THAT SHIT!!!
;;;
;;;   INPUTS:   POPULATION, vector of INDIVIDUALS
;;;             NEW-PROGS,
;;;             (fitness-cases)
;;;             MAXIMUM-GENERATION
;;;             FITNESS-FUNCTION
;;;             TERMINATION-PREDICATE
;;;             FUNCTION-SET
;;;             ARGUMENT-MAP
;;;             TERMINAL-SET
;;;   OUTPUT:   Victory

(defun execute-generations (population new-programs max-generations
                fitness-function termination-predicate function-set
                argument-map terminal-set)
 "Runs that shit!!!!  Loops intil the termination predicate 
  says to stop or MAX-GENERATIONS is reached."
                 
 (do ((current-generation 0 (+ 1 current-generation)))
     ;; Looop incrementing current generation until 
     ;; termination predicate succedes.
     ((let ((best-of-generation (svref population 0)))
        (funcall termination-predicate 
                  current-generation max-generations
                  (individual-standardized-fitness best-of-generation))))
     
     (when (> current-generation 0)
      ;; Breed the new pop for this (non-zero) generation
      (breed-new-pop population new-programs function-set
                     argument-map terminal-set))
     
     ;; Evaluate fitness
     (evaluate-pop-fitness population fitness-function)     
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
     
     ;; Print statistics ???
     ;; :P
     
     ))


;;;============================================================
;;;  TERMINATOR
;;;============================================================
;;; Returns a boolean for whether to end or not

(defun terminator (current-gen max-gens best-indi-fitness)
 (if (or (>= current-gen max-gens) (= best-indi-fitness 1.0))
    true
  ;; Else
  nil)


        
                                                 




;;;************************************************************
;;;   TESTS
;;;************************************************************

(defvar *f-set* '(+ - * %))

(defvar *a-map* '(2 2 2 2))

(defvar *t-set* '(1 2 3 4 5))

(defvar *pop-size* 10)


;;; TESTS for CREATE-POPULATION
;;;--------------------------------
;;(print-test-header "CREATE-POPULATION")
;;(tester '(create-population *pop-size* *f-set* *a-map* *t-set*))

;;(defvar *test-pop* (create-population *pop-size* *f-set* *a-map* *t-set*))



;;; TESTS for EVALUATE-POP-FITNESS
;;;--------------------------------
;;(print-test-header "EVELUATE-POP-FITNESS")
;;(tester '(evaluate-pop-fitness *test-pop* #'eval))

;;(defvar *test-evaluated-pop* (evaluate-pop-fitness *test-pop* #'eval))


;;; TESTS for NORMALIZED-POPULATION-FITNESS
;;;----------------------------------------
;;(print-test-header "NORMALIZED-POP-FITNESS")

;;(defvar *test-normalized-pop* (normalize-population-fitness *test-evaluated-pop*))

;;; TESTS for SORT-POP-BY-FITNESS
;;;-------------------------------
;;(print-test-header "SORT-POP-BY-FITNESS")
;;(tester '(sort-pop-by-fitness *test-normalized-pop*))