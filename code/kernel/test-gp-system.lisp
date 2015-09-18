;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  test-gp-system.lisp
;;; ========================================
;;; File containing tests and functions to run the 
;;; genetic-programming system for the stock prediction
;;; problem.


;;; System Stuff???
;;;==========================================================

;;; Set the global garbage collection behavior to increase the
;;; allowed number of tenured bytes by a factor of 100 over the 
;;; default.  This is because global garbage collection is
;;; slow, and the fewer global-gcs have to be done the faster
;;; the algorithm can run.
(setf *tenured-bytes-limit* 338800000)    ;; ~ 323 MB



;;; Stuff to Load
;;;==========================================================

;; Kernel
(load (compile-file "kernel-5"))

;; Randomizer functions
;;(load (compile-file "randomizer"))

;; Reporting functions
;;(load (compile-file "reporters"))

;; Problem-Specific Code
(load (compile-file "problem-code-3"))




;;;************************************************************
;;;   DATE-TABLE
;;;************************************************************

(defvar *date-table* (make-hash-table :test #'equal))


;;;************************************************************
;;;   TEST FUNCTIONS
;;;************************************************************

;;;=============================================================
;;; MAKE-DATE-TABLE
;;;=============================================================
;;; Should make a hash table of dates corresponding to line
;;; numbers in data.txt

(defun make-date-table ()
  (with-open-file (stream "date-range.txt")
    ;; Date-range.txt has 12897 records
    (dotimes (i 12896)
      (let ((line (read stream)))
	(setf (gethash (second line) *date-table*) (first line))))))

;; Run MAKE-DATE-TABLE
(make-date-table)


;;;=============================================================
;;;  TEST 1
;;;=============================================================

(defun test1 ()
  ;; Clear out *DATA-ARRAY*
  (setf *data-array* nil)
  
  ;; Create the *DATA-ARRAY*
  (create-data-array 31 111 10)
  ;; Run GP
  (run-gp-system 'stock-prediction 1.0 50 200))


;;;=============================================================
;;; TEST2
;;;=============================================================
;;; By default runs the GP-System on NUM-CASES starting with 
;;; the date START.  If an end date is specified, it will
;;; choose NUM-CASES random dates in the given range.

;;; Earliest start date: "2/12/1962"
;;; Latest End date:     "5/8/2013"


;;;   INPUTS:   START, string
;;;             NUM-CASES, int
;;;             :END, string
;;;             Optional: 
;;;                MAX-GENS, int
;;;                POP-SIZE, int
;;;   OUTPUT:   NONE
;;;   SIDE-FX:  Runs teh GP-system on the stock prediction
;;;             problem.

(defun test2 (start num-cases &optional (max-gens 75) (pop-size 500)
	      &key (end nil))
   
  ;; Check input
  (assert (gethash start *date-table*)
      (start)
    "It looks like the market was closed on ~A.
You should try another day." start)

  (let* ((s-date (gethash start *date-table*))
	 ;; If used doesn't specify END set it to 
	 ;; NUM-CASES + s-date, if it is set, get the hash 
	 (e-date (if (null end) (+ num-cases s-date) 
		   ;; Else
		   (gethash end *date-table*))))
        
    ;; Clear out *DATA-ARRAY*
    (setf *data-array* nil) 
    ;; Create the *DATA-ARRAY*
    (create-data-array s-date e-date num-cases)

    ;; Run GP System    
    ;; Get runtime data
    (let* ((start (get-internal-real-time))
	   ;; call the kernel (even though it desn't return anything)
	   (result (run-gp-system 'stock-prediction 
				  1.0 max-gens pop-size))
	   (elapsed-time (- (get-internal-real-time) start)))
      
      (format t "~2%  COPMLETED RUN!!!")
      (format t "~%============================================~%")
      (let ((total-secs (* 1.0
			   (/ elapsed-time 
			      internal-time-units-per-second))))
	(multiple-value-bind (mins secs)
	    (floor total-secs 60)
	  (format t " Run time: ~A minutes ~A seconds~%" mins secs))))
	      		     
      
    ;; Print BEST-OF-RUN
    (format t "~2% Best of run Individual:")
    (format t "~%============================================~%")
    *best-of-run-individual*))
