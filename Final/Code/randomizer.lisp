;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  randomizer.lisp
;;; ========================================
;;; This is a file contains the functions for random
;;; number generation used by the kernel.  They are based on
;;; the Park-Miller randomizer algorithm.

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
;;; park-miller-randomizer
;;; random-floating-point-number
;;; random-integer

  
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


;;;============================================================
;;;  PARK-MILLER-RANDOMIZER
;;;============================================================

(defun park-miller-randomizer ()
  "The Park-Miller multiplicative congruential randomizer.
   Creates psudo random floating point numbers in the range 
   0.0 < x < 1.0.  Records the seed value, *SEED*, to make
   sure the runs are reproducable."
  
  #+Lucid (unless (typep *seed* 'integer)
            (setq *seed* (round *seed*)))
  
  (assert (not (zerop *seed*)) () "*seed* cannot be zero.")
  
  (let* ((multiplier #+Lucid 16807 #-Lucid 16807.0d0)
	 (modulus #+Lucid 2147483647 #-Lucid 2147483647.0d0)
	 (temp (* multiplier *seed*)))
   
    (setf *seed* (mod temp modulus))
    
    (#+lucid float #-lucid progn (/ *seed* modulus))))


;;; RANDOM-FLOATING-POINT-NUMBER
;;;============================================================
(defun random-floating-point-number (n)
  "Returns a pseudo random floating-point number
  in range 0.0 <= number < n" 
  (let ((random-number (park-miller-randomizer)))
    ;; We subtract the randomly generated number from 1.0
    ;; before scaling so that we end up in the range
    ;; 0.0 <= x < 1.0, not 0.0 < x <= 1.0
    (* n (- 1.0d0 random-number))))


;;; RANDOM-INTEGER
;;;============================================================
(defun random-integer (n)
  "Returns a pseudo-random integer in the range 0 ---> n-l."
  (let ((random-number (random-floating-point-number 1.0)))
    (floor (* n random-number))))