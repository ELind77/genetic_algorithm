;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  reporters.lisp
;;; ========================================
;;; This file contains the functions used to generate nice
;;; reports for the running of the genetic algorithm.

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
;;; report-on-run
;;; report-on-generation-log
;;; report-on-generation
;;; print-population
;;; describe-parameters-for-run


;;;============================================================
;;;  REPORT-ON-RUN
;;;============================================================

(defun report-on-run (&key (stream t))
  "Prints out the best-of-run individual."
  (let ((*print-pretty* t))
    (format stream "~2%Best of run individual program:~%")
    (format stream " Generation:       ~A~%" 
	    *generation-of-best-of-run-individual*))

  (format stream "~A~%" *best-of-run-individual*)
  )


;;;============================================================
;;;  REPORT-ON-GENERATION-LOG
;;;============================================================

(defun report-on-generation-log (generation-number population &key (stream t))
  "Prints out the best individual at the end of each generation"
  (let ((best-individual (aref population 0))
	(size-of-population (length population))
	(sum 0.0)
	(*print-pretty* t))
    ;; Add up all of the standardized fitnesses to get average
    (dotimes (index size-of-population)
      (incf sum (individual-standardized-fitness-percent-avg
		 (aref population index))))
      
       (format stream "~%Generation ~D: Average standardized-fitness ~
= ~S. ~%~
The best individual program of the population ~
had a ~%standardized fitness measure of ~D ~
and ~D hit~P. ~%It was: ~%~S~%"
	       generation-number (/ sum (length population))
	       (individual-standardized-fitness-percent-avg best-individual)
	       (individual-hits best-individual)
	       (individual-hits best-individual)
	       (individual-program best-individual))))


;;;============================================================
;;;  REPORT-ON-GENERATION
;;;============================================================

(defun report-on-generation (generation-number population)
  "Prints out the best individual at the end of each generation"
  (let ((best-individual (aref population 0))
	(size-of-population (length population))
	(sum 0.0)
	(*print-pretty* t))
    ;; Add up all of the standardized fitnesses to get average
    (dotimes (index size-of-population)
      (incf sum (individual-standardized-fitness-percent-avg
		 (aref population index))))
    
    (format t "~2%Generation ~D: Average standardized-fitness ~
= ~S. ~%~
The best individual program of the population ~
had a ~%standardized fitness measure of ~D ~
and ~D hit~P. ~%It was: ~%~S"
	    generation-number (/ sum (length population))
	    (individual-standardized-fitness-percent-avg best-individual)
	    (individual-hits best-individual)
	    (individual-hits best-individual)
	    (individual-program best-individual))))



;;;============================================================
;;;  PRINT-POPULATION
;;;============================================================

(defun print-population (population)
  "Given a population, this prints it out (for debugging) "
  (let ((*print-pretty* t))
    (dotimes (index (length population))
      (let ((individual (aref population index)))
	(format t "~&~D ~S ~S"
		index
		(individual-standardized-fitness individual)
		(individual-program individual))))))


;;;============================================================
;;;  DESCRIBE-PARAMETERS-FOR-RUN
;;;============================================================

(defun describe-parameters-for-run
    (maximum-generations size-of-population seed &key (stream t))
  "Lists the parameter settings for this run."
  (format stream "~2%Parameters used for this run.~
~%=================================================")
  (format stream "~%Number of fitness cases:~50T~D"
	  *num-fitness-cases*)
  (format stream "~%Maximum number of Generations:~50T~D"
	  maximum-generations)
  (format stream "~%Size of Population:~50T~D" size-of-population)
  (format stream "~%Maximum depth of new individuals:~50T~D"
	  *max-depth-for-new-indis*)
  (format stream "~%Maximum depth of new subtrees for mutants:~50T~D"
	  *max-depth-for-new-subtrees-in-mutants*)
  (format stream
	  "~%Maximum depth of individuals after crossover:~50T~D"
	  *max-depth-for-individuals-after-crossover*)
  (format stream
	  "~%Fitness-proportionate reproduction fraction:~50T~D"
	  *fitness-proportionate-reproduction-fraction*)
  (format stream "~%Crossover at any point fraction:~50T~D"
	  *crossover-at-any-point-fraction*)
  (format stream "~%Crossover at function points fraction:~50T~D"
	  *crossover-at-function-point-fraction*)
  (format stream "~%Selection method: ~50T~A" *method-of-selection*)
  (format stream "~%Generation method: ~50T~A" *method-of-generation*)
  (format stream "~%Randomizer seed: ~50T~D" seed))
