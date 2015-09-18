;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  problem-code.lisp
;;; ========================================
;;; This file contains the problem-specific code 
;;; necessary to run the genetic-programming system.
;;; It contains functions that provide information to the
;;; kernel in order to run the system. 

;;; Notes on this version
;;;--------------------------------------------------------
;;; This version of the PROBLEM-CODE uses the FAST-EVAL 
;;; function and macro defi9ned in Koza's book for faster 
;;; evaluation.

;;; ACKNOWLEDGMENT:
;;;----------------------------------------------------------------
;;;The basis for these functions was the simple
;;; LISP code in John R. Koza's book: Genetic Programming:
;;; On the Programming of Computers by Means of Natural Selection.
;;; The simple LISP kernel is in Appendix C, and has been approved
;;; for educational and personal use.
;;;----------------------------------------------------------------


;;; FUNCTIONS in this file
;;;===========================================================
;;; create-data-array
;;; define-terminal-set
;;; define-function-set
;;; define-fitness-cases
;;; evaluate-fitness-for-stock-prediction
;;; define-parameters-for-stock-prediction
;;; terminator
;;; stock-prediction
;;; fast-eval-fun
;;; fast-eval


;;;************************************************************
;;;   GLOBALS
;;;************************************************************

;;; Run Parameters
;;;----------------------------
;;; Earlier version had these fixed.  However, I would 
;;; like the user to be able to set these interactively 
;;; during testing.

(defvar *function-set*)

(defvar *terminal-set*)


;;; Terminals
;;;----------------------------
;;(defvar *stock-num*)

(defvar *stock-date*)

(defvar *stock-open*)

(defvar *stock-high*)

(defvar *stock-low*)

(defvar *stock-close*)

(defvar *stock-adj-close*)

(defvar *stock-volume*)

(defvar *stock-7-day*)

(defvar *stock-30-day*)

(defvar *stock-3-slope*)


;;; *DATA-ARRAY*
;;;----------------------------
;;; An array to hold the data for the fitness cases for 
;;; the current run.
(defvar *data-array* nil)



;;;************************************************************
;;;   MY-FUNCTIONS
;;;************************************************************


;;;============================================================
;;;  CREATE-DATA-ARRAY
;;;============================================================
;;; Calls an external BASH script (trial-maker.sh) to select 
;;; NUM-FITNESS-CASES lines from data.txt in order to make a 
;;; trial.txt file and then creates an array from trial.txt.
;;;
;;;   INPUTS:   START, int
;;;             END, int
;;              NUM-FITNESS-CASES, int
;;;   OUTPUT:   NONE
;;;   SIDE-FX:  Sets values for *DATA-ARRAY*

(defun create-data-array (start end num-fitness-cases)
  "Creates an array of data for use with this genetic programming
system."
  
  ;; Create file to hold trial parameters
  (with-open-file (stream "trial_parameters.txt" :direction :output
		   :if-exists :supersede)
    (format stream "~A ~A ~A" start end num-fitness-cases))
  
  ;; Create the new trial data in trial.txt
  (run-shell-command "./trial-maker2.sh `cat trial_parameters.txt`")
  
  ;; Create the array
  (let ((data-array (make-array (list num-fitness-cases 12))))
    ;; Open the file stream
    (with-open-file (stream "trial.txt")
      
      ;; Walk through the file line-by-line
      (dotimes (i num-fitness-cases)
	;; Hold the current line
	(let ((current-line (read stream)))
	  ;; Now walk through each line
	  (dotimes (j 12)
	    (setf (aref data-array i j) (elt current-line j))))))
    
    ;; Set the array
    ;; A PROGN is used here to suppress the output of SETF
    ;; so that the *DATA-ARRAY* doesn't print to the console.
    (progn
      (setf *data-array* data-array)
      nil)
    ))



;;;************************************************************
;;;   Koza-FUNCTIONS
;;;************************************************************

;;;============================================================
;;;  DEFINE-TERMINAL-SET
;;;============================================================
;;; Defines the terminal set for a problem
;;;   INPUTS:   NONE
;;;   OUTPUT:   A list of terminals

(defun define-terminal-set ()
  *terminal-set*)


;;;============================================================
;;;  DEFINE-FUNCTION-SET
;;;============================================================
;;; Defines the terminal set for a problem
;;;   INPUTS:   NONE
;;;   OUTPUT:   A list of functions
;;;             A list of ints to be used as the argument map

(defun define-function-set ()
  ;;       (+ - * % srt rlog rsin rsinh rtan pro-exp pro-expt ratan)
  (values (first *function-set*) (second *function-set*)))


;;;============================================================
;;;  FITNESS-CASE STRUCT
;;;============================================================
;;; The struct to represent a fitness case

(defstruct fitness-case
  num					
  date			      ;;The date of trading
  open			      ;;The opening price
  high			      ;;The high
  low			      ;;The low
  close			      ;;The closing price
  adj-close		      ;;The adjusted closing change
  volume		      ;;The trading vloume
  7-day			      ;;The 7-day average closing price
  30-day		      ;;The 30-day average closing price
  3-day-slope		      ;;Slope of the closing over the last 3 days
  goal)			      ;;The closing price on the following day


;;;============================================================
;;;  DEFINE-FITNESS-CASES
;;;============================================================
;;; Defines the fitness cases for a problem
;;;   INPUTS:   NONE
;;;   OUTPUT:   A vector of fitness cases

(defun define-fitness-cases ()
  (let ((fitness-cases (make-array *num-fitness-cases*)))
    
    ;; Walk through *DATA-ARRAY* rows
    (dotimes (i *num-fitness-cases*)
      ;; Hold a list of struct parameters
      (let ((field-vec (make-array (array-dimension *data-array* 1))))
			
	;; Walk through *DATA-ARRAY* columns for field values
	(dotimes (j (length field-vec))	  
	  ;; Set the fields
	  (setf (svref field-vec j)
	    (aref *data-array* i j)))
	
	;; Now that we're done with the row, make the new struct
	;; and add it to the FITNESS-CASES
	(setf (svref fitness-cases i) 
	  (make-fitness-case 
	   :num (svref field-vec 0)
	   :date (svref field-vec 1)
	   :open (svref field-vec 2)
	   :high (svref field-vec 3)
	   :low (svref field-vec 4)
	   :close (svref field-vec 5)
	   :adj-close (svref field-vec 6)
	   :volume (svref field-vec 7)
	   :7-day (svref field-vec 8)
	   :30-day (svref field-vec 9)
	   :3-day-slope (svref field-vec 10)
	   :goal (svref field-vec 11)))))
    
    ;; Return FITNESS-CASES
    fitness-cases))


;;;============================================================
;;;  EVALUATE-STANDARDIZED-FITNESS-FOR-STOCK-PREDICTION
;;;============================================================
;;; Evaluates the standardized fitness of a given individual
;;; for the stock price prediction problem.  The RAW-FITNESS for 
;;; each fitness case is calculated by using EVAL on the given 
;;; PROGRAM and then stored in the RAW-FITNESS-VECTOR, which is
;;; is returned as the RAW-FITNESS for the program.  The 
;;; STANDARDIZED-FITNESS for each case is calculated by taking 
;;; the absolute value of the difference between the RAW-FITNESS 
;;; and the GOAL for the current fitness case (the error), and 
;;; that is added to a vector (mostly for debugging). Then the 
;;; STANDARDIZED-FITNESS for the PROGRAM is the sum of the entries
;;; in the vector (sum of the errors).
;;;
;;;   INPUTS:   PROGRAM, an S-expression
;;;             FITNESS-CASES, a vector of fitness-case structs
;;;   OUTPUTS:  RAW-FITNESS, vactor of ints
;;;             STANDARD-FITNESS-VECTOR, simple-vector
;;;             SUM-OF-STANDARDIZED-FITNESS, float (probably)
;;;             Average standardized fitness
;;;             average standardized fitness as a percentage of GOAL
;;;             HITS, int

(defun evaluate-fitness-for-stock-prediction (program fitness-cases)
  "Evaluates the standardized fitness of a given individual 
for the stock price prediction problem.  The RAW-FITNESS for 
each fitness case is calculated by using EVAL on the given 
PROGRAM and then stored in the RAW-FITNESS-VECTOR, which is returned
as the RAW-FITNESS for the program.  The STANDARDIZED-FITNESS for 
each case is calculated by taking the absolute value of the 
difference between the RAW-FITNESS and the GOAL for the current 
fitness case [the error], and that is added to a vector 
[mostly for debugging]. The STANDARDIZED-FITNESS for the PROGRAM 
is the sum of the entries in the vector [sum of the errors]."
  
  ;; Store the HITS so they can be incremented
  (let ((raw-fitness-vector (make-array (length fitness-cases)))
	(standard-fitness-vector (make-array (length fitness-cases)))
	(standard-fit-percent-vector 
	 (make-array (length fitness-cases)))
	(sum-of-standardized-fitnesses 0.0)
	(sum-of-standardized-percent 0.0)
	(hits 0))
    ;; Walk through the fitness cases
    (dotimes (index (length fitness-cases))
      ;; Store relevant values in a local
      (let* ((this-fitness-case (svref fitness-cases index))
	     (goal (fitness-case-goal this-fitness-case))
	     ;; Set the values for all of the terminals so
	     ;; that they can be used by EVAL
	     (*stock-open* (fitness-case-open this-fitness-case))
	     (*stock-high* (fitness-case-high this-fitness-case))
	     (*stock-low* (fitness-case-low this-fitness-case))
	     (*stock-close* (fitness-case-close this-fitness-case))
	     (*stock-adj-close* (fitness-case-adj-close this-fitness-case))
	     (*stock-volume* (fitness-case-volume this-fitness-case))
	     (*stock-7-day* (fitness-case-7-day this-fitness-case))
	     (*stock-30-day* (fitness-case-30-day this-fitness-case))
	     (*stock-3-slope* (fitness-case-3-day-slope this-fitness-case))
	     ;; Evaluate fitnesses
	     ;;(raw-fitness (coerce (eval program) 'integer))
	     (raw-fitness (fast-eval program))
	     (standardized-fitness (abs (- goal raw-fitness)))
	     ;; Error as a percent of the goal
	     (percent-diff (* (/ standardized-fitness goal) 100.0)))
	
	;; Add values to vectors
	(setf (svref raw-fitness-vector index) raw-fitness)
	(setf (svref standard-fitness-vector index) 
	  standardized-fitness)
	(setf (svref standard-fit-percent-vector index)
	  percent-diff)
	
	;; Increment the SUM-OF-STANDARDIZED-FITNESSES and PERCENT
	(incf sum-of-standardized-fitnesses standardized-fitness)
	(incf sum-of-standardized-percent percent-diff)
	
	;; Increment the hits if the criterion is met
	(when (<= percent-diff 1) (incf hits))))
	
    ;; Return the fitnesses and hits
    (let ((std-fitness-avg (/ sum-of-standardized-fitnesses 
			      (length fitness-cases)))
	  (std-fitness-percent-avg (/ sum-of-standardized-percent
				      (length fitness-cases))))
      
      (values raw-fitness-vector
	      standard-fitness-vector 
	      standard-fit-percent-vector
	      sum-of-standardized-fitnesses
	      std-fitness-avg
	      std-fitness-percent-avg
	      hits))))


;;;============================================================
;;;  DEFINE-PARAMETERS-FOR-STOCK-PREDICTION
;;;============================================================
;;; Defines the parameters for running the genetic programming
;;; system.
;;;
;;; problem
;;;   INPUTS:   NONE
;;;   OUTPUT:   NONE
;;;   SIDE-FX:  Sets values for running the genetic programming 
;;;             system.

(defun define-parameters-for-stock-prediction ()

  (setf *num-fitness-cases* (array-dimension *data-array* 0))
  (setf *max-depth-for-new-indis* 6)	;6
  (setf *max-depth-for-individuals-after-crossover* 30) ;17
  (setf *fitness-proportionate-reproduction-fraction* 0.01) ;0.1
  (setf *crossover-at-any-point-fraction* 0.2) ;0.2
  (setf *crossover-at-function-point-fraction* 0.7) ;0.7
  (setf *max-depth-for-new-subtrees-in-mutants* 6) ;4
  (setf *method-of-selection* :fitness-proportionate)
  (setf *method-of-generation* :ramped))


;;;============================================================
;;;  TERMINATOR
;;;============================================================
;;; Used to determine if we should stop the current run of the 
;;; GP-system.  Returns T is the system should stop. 
;;;
;;;   INPUTS:   CURRENT-GENERATION, int
;;;             MAXIMUM-GENERATIONS, int
;;;             BEST-STANDARDIZED-FITNESS
;;;             BEST-HITS
;;;   OUTPUT:   BOOLEAN

(defun terminator (current-generation maximum-generations
		   best-standardized-fitness best-hits)
  
  (or (>= current-generation maximum-generations)
      ;;(<= best-standardized-fitness 5)
      (>= best-hits *num-fitness-cases*)
      ))


;;;============================================================
;;;  STOCK-PREDICTION
;;;============================================================
;;; Used to tell the kernel about all of the problem-specific
;;; functions
;;;
;;;   INPUTS:   NONE
;;;   OUTPUT:   DEFINE-FUNCTION-SET
;;;             DEFINE-TERMINAL-SET
;;;             DEFINE-FITNESS-CASES
;;;             EVALUATE-FITNESS-FOR-STOCK-PREDICTION
;;;             DEFINE-PARAMETERS-FOR-STOCK-PREDICTION
;;;             TERMINATOR

(defun stock-prediction ()
  (values 'define-function-set
	  'define-terminal-set
	  'define-fitness-cases
	  'evaluate-fitness-for-stock-prediction
	  'define-parameters-for-stock-prediction
	  'terminator))



;;;============================================================
;;;  FAST-EVAL-FUN
;;;============================================================
;;; A macro to replace the standard EVAL function in order to 
;;; decrease run times.

(defmacro fast-eval-fun ()
  "A code body that does fast evaluation of a
functional expression."
  '(ecase (length expr)
    (1 (funcall fef))
    (2 (funcall fef
	(fast-eval
	 (second expr))))
    (3 (funcall fef
	(fast-eval (second expr))
	(fast-eval
	 (third expr))))
    (4 (funcall fef
	(fast-eval (second expr))
	(fast-eval (third expr))
	(fast-eval (fourth expr))))
    (5 (funcall fef
	(fast-eval (second expr))
	(fast-eval (third expr))
	(fast-eval (fourth expr))
	(fast-eval (fifth expr))))
    (6 (funcall fef
	(fast-eval (second expr))
	(fast-eval (third expr))
	(fast-eval (fourth expr))
	(fast-eval (fifth expr))
	(fast-eval (fifth expr))
	(fast-eval (sixth expr))))
    (7 (funcall fef
	(fast-eval (second expr))
	(fast-eval (third expr))
	(fast-eval (fourth expr))
	(fast-eval (fifth expr))
	(fast-eval (sixth expr))
	(fast-eval (seventh expr))))))


;;;============================================================
;;;  FAST-EVAL
;;;============================================================
(defun fast-eval (expr)
  "A fast evaluator that can be used with the
Genetic Programming Paradigm for Lucid and Franz Lisps."
  (cond ((consp expr)
	 (let ((function (first expr)))
	   (if (eq 'quote function)
	       (second expr)
	     (let ((fef (symbol-function function)))
	       (cond ((compiled-function-p fef)
		      (fast-eval-fun))
		     ;; Then ASSUME we are a pseudo
		     ;; macro and are bound.
		     (t (apply (symbol-value function)
			       (rest expr))))))))
	((symbolp expr) (symbol-value expr))
	(t expr)))
