========================================
 CMPU-365, Spring 2013
 Final Project: Genetic Programing
 Name: Eric Lind
========================================
 FILE:  README.txt
========================================

README file for Eric's Genetic Programming system.


How to use the code:


Files necessary for a run:
==================================

kernel.lisp 
problem-code.lisp       
test-gp-system.lisp  
randomizer.lisp           
reporters.lisp 
protected-functions.lisp  

trial-maker2.sh   

data.txt        
trial_parameters.txt


Instructions:
=====================================

In order to run the genetic programming system on the stock prediction problem
compile and load test-gp-system.lisp which will load and compile all other 
necessary functions.

To performs a run, call TEST-GP-SYSTEM, the contract for the function is below:

;;;=============================================================
;;;  TEST-GP-SYSTEM
;;;=============================================================
;;; By default runs the GP-System on NUM-CASES starting with 
;;; the date START.  If an end date is specified, it will
;;; choose NUM-CASES random dates in the given range.

;;; Earliest start date: "2/12/1962"
;;; Latest End date:     "5/8/2013"


;;;   INPUTS:   START, string
;;;             NUM-CASES, int
;;;             :MAX-GENS (75) int
;;;             :POP-SIZE, (500) int
;;;             :END (nil), string
;;;             :VARBOSE? (nil), boolean
;;;             :SEED (1.0), float
;;;             :LOG? (nil), boolean
;;;             :LOG-FILE ("gp-log.txt"), sting
;;;             :CLEAR-LOG? (t),  boolean
;;;             :F-SET, (values (list + - * % srt rlog rsin)
;;;                             (list 2 2 2 2 1   1    1))
;;;             :T-SET, (list *stock-open* *stock-high* *stock-low*
;;;                      *stock-close* *stock-adj-close* *stock-volume*
;;;                      *stock-7-day* *stock-30-day* *stock-3-slope*)
;;;   OUTPUT:   NONE
;;;   SIDE-FX:  Runs the GP-system on the stock prediction
;;;             problem.


The only mandatory arguments are a start date which is a string representing a
date between 2/12/1962 and 5/8/2013, and a number of fitness cases.  The start 
string should be in the form "M/D/YYYY".  If an end date is specified, and the
number of fitness-cases given is less than the number of days with data in the 
range from start-end then the program will randomly choose a NUM-CAESES dates
in the range given. 

All of the other arguments are keyword parameters.  The defualt for MAX-GENS is 
75, but better results can be found in the range 150-200.

The default value for POP-SIZE is 500 which will yield reasonably good results 
for any time period but larger populations can be better (especially with the 
current setting of 0.09 mutant fraction per generation)

The SEED variable is set to 1.0 by default to produce reproducable results and 
doesn't need to be changed.

If LOG? is set to true but no log file is specified, it will log to 
gp-log.txt by default.  But if the system is run more than once it will erase 
that log file by default.  Or append to it if CLEAR-LOG? is set to NIL.

If you are interested in changing the fraction of mutants you need to change 
the values in the DEFINE-PARAMETERS-FOR-STOCK-PREDICTION function in the
problem-code.lisp file.

Beyond that, the rest is up to you.


Eric Lind

