;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  reporters.lisp
;;; ========================================

;;; This file contains the functions used to generate nice
;;; reports for the running of the genetic algorithm.



;;;============================================================
;;;  REPORT-ON-RUN
;;;============================================================

(defun report-on-run ()
  "Prints out the best-of-run individual."
  (let ((*print-pretty* t))
    (format t "~3%The best-of-run individual program ~
for this run was found on ~%generation ~D and had a ~
standardized fitness measure of ~D and ~D hit~P. ~%It was:~%~S"
	    *generation-of-best-of-run-individual*
	    (individual-standardized-fitness *best-of-run-individual*)
	    (individual-hits *best-of-run-individual*)
	    (individual-hits *best-of-run-individual*)
	    (individual-program *best-of-run-individual*))))


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
      (incf sum (individual-standardized-fitness
		 (aref population index))))
    (format t "~2%Generation ~D: Average standardized-fitness ~
= ~S. ~%~
The best individual program of the population ~
had a ~%standardized fitness measure of ~D ~
and ~D hit~P. ~%It was: ~%~S"
	    generation-number (/ sum (length population))
	    (individual-standardized-fitness best-individual)
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



(defun describe-parameters-for-run
    (maximum-generations size-of-population)
  "Lists the parameter settings for this run."
  (format t "~2%Parameters used for this run.~
~%=============================")
  (format t "~%Maximum number of Generations:~50T~D"
	  maximum-generations)
  (format t "~%Size of Population:~50T~D" size-of-population)
  (format t "~%Maximum depth of new individuals:~50T~D"
	  *max-depth-for-new-indis*)
  (format t "~%Maximum depth of new subtrees for mutants:~50T~D"
	  *max-depth-for-new-subtrees-in-mutants*)
  (format t
	  "~%Maximum depth of individuals after crossover:~50T~D"
	  *max-depth-for-individuals-after-crossover*)
  (format t
	  "~%Fitness-proportionate reproduction fraction:~50T~D"
	  *fitness-proportionate-reproduction-fraction*)
  (format t "~%Crossover at any point fraction:~50T~D"
	  *crossover-at-any-point-fraction*)
  (format t "~%Crossover at function points fraction:~50T~D"
	  *crossover-at-function-point-fraction*)
  (format t "~%Number of fitness cases:~50T~D"
	  *num-fitness-cases*)
  (format t "~%Selection method: ~50T~A" *method-of-selection*)
  (format t "~%Generation method: ~50T~A" *method-of-generation*)
  (format t "~%Randomizer seed: ~50T~D" *seed*))
