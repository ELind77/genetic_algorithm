;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  test-gp-system.lisp
;;; ========================================
;;; This file contains tests and functions to run the 
;;; genetic-programming system for the stock prediction
;;; problem.


;;; FUNCTIONS in this file
;;;===========================================================
;;; make-date-table
;;; test1
;;; test-function-looper
;;; test-terminal-looper
;;; test-gp-system


;;; System Stuff
;;;==========================================================
;;; Set the global garbage collection behavior to increase the
;;; allowed number of tenured bytes by a factor of 100 over the 
;;; default.  This is because global garbage collection is
;;; slow, and the fewer global-gcs have to be done the faster
;;; the algorithm can run.

;;; WARNING!!!
;;; Increasing the tenured-bytes variable may make the 
;;; LISP system unstable (or it could be something else).
(setf *tenured-bytes-limit* 338800000)    ;; ~ 323 MB



;;; Stuff to Load
;;;==========================================================

;; Kernel
(load (compile-file "kernel"))

;; Problem-Specific Code
(load (compile-file "problem-code"))




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
;;;  TEST-FUNCTION-LOOPER
;;;=============================================================
;;; Tester to walk through a given function set to produce output
;;; to show how fitness changes as functions are added.
;;; 
;;;   INPUTS:   START-DATE, string
;;;             F-SET, list
;;;             A-MAP, list   NOTE: A-MAP must be an argument map for
;;;                                 F-SET!
;;;   OUTPUTS:  NONE
;;;   SIDE-FX:  Writes output to test-function-looper.txt

(defun test-function-looper (start-date f-set a-map)
   
  (dotimes (i (length f-set))
    (let ((temp-f-set (subseq f-set 0 (+ i 1)))
	  (temp-a-map (subseq a-map 0 (+ i 1))))
      (test-gp-system start-date 30 :max-gens 100 :log? t 
		      :clear-log? nil
		      :log-file "test-function-looper.txt"
		      :f-set (list temp-f-set temp-a-map)))))


;;;=============================================================
;;;  TEST-TERMINAL-LOOPER
;;;=============================================================
;;; Tester to walk through a given function set to produce output
;;; to show how fitness changes as functions are added.
;;; 
;;;   INPUTS:   START-DATE, string
;;;             F-SET, list
;;;   OUTPUTS:  NONE
;;;   SIDE-FX:  Writes output to test-terminal-looper.txt

(defun test-terminal-looper (start-date t-set)
  
  (dotimes (i (length t-set))
    (let ((temp-t-set (subseq t-set 0 (+ i 1))))
      (test-gp-system start-date 30 :max-gens 100 :log? t 
		      :clear-log? nil
		      :log-file "test-terminal-looper.txt"
		      :t-set temp-t-set))))


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

(defun test-gp-system (start num-cases 
		       &key (max-gens 75) (pop-size 500) 
			    (end nil) (verbose? nil) 
			    (seed 1.0) (log? nil)
			    (log-file "gp-log.txt")
			    (clear-log? t)
			    (f-set (list '(+ - * % srt rlog rsin)
					 '(2 2 2 2 1   1    1)))
			    (t-set '(*stock-open* *stock-high* *stock-low*
				     *stock-close* *stock-adj-close* *stock-volume*
				     *stock-7-day* *stock-30-day* *stock-3-slope*)))
   
  ;; Check input
  (assert (gethash start *date-table*)
      (start)
    "It looks like the market was closed on ~A.
You should try another day." start)

  (if end
      (assert (and (gethash end *date-table*)
		   (<= num-cases
		       (- (gethash end *date-table*)
			  (gethash start *date-table*))))
	  (end)
	"You have chosed a bad value for END. 
Either you have chosen a day with no data or 
you have specified NUM-CASES out of the range 
of START to END.  Please check your dates." end))  
  
  ;; Set an end date if not set
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
    
    ;; Set *F-SET* and *T-SET*
    (setf *function-set* f-set)
    (setf *terminal-set* t-set)    
    
    ;; Notify user that run is starting
    (cond
     ((>= max-gens 500)
      (format t "The run is about to start. 
If You're running more than 500 individuals you may want to 
go and get some coffee or something because this could take
a while.~%"))
     ((>= (* max-gens num-cases pop-size) 2000000)
      (format t "WARNING: With the current settings, a full
run of the GP system wil require evaluating as many as, or
more than, 2000000 functions.  This could really take a while.
You may want to get a good book if you you plan on waiting 
for the output.~%"))
     (t
      (format t "Genetic Program commencing run!~2%")))
    
    ;; Clear old log file if it exists
    (if clear-log?
	;; If an old logfile exists, clear it
	;; If it doesn't, this will create it too :-)
	(with-open-file (stream log-file :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
	  (fresh-line stream)))
    
    ;; Verbose mode
    (if verbose?
	(format t "Running in VERBOSE mode.~%"))
    
    ;; Run GP System    
    ;; Get runtime data
    (let* ((start (get-internal-real-time))
	   ;; call the kernel (even though it desn't return anything)
	   (result (run-gp-system 'stock-prediction 
				  seed 
				  max-gens 
				  pop-size
				  :verbose? verbose?
				  :log? log?
				  :log-file log-file))
	   (elapsed-time (- (get-internal-real-time) start)))
      
      ;; Actual Logging
      (if log? 
	  (progn
	    (format t "Writing log to: ~A~%" log-file)
	    (with-open-file (stream log-file :direction :output
			     :if-exists :append)
	      (describe-parameters-for-run max-gens pop-size seed 
					   :stream stream)
	      (report-on-run :stream stream))))
      
      ;; Print statistics
      
      (format t "~2%  COPMLETED RUN!!!")
      (format t "~%================================================~%")
      (let ((total-secs (* 1.0
			   (/ elapsed-time 
			      internal-time-units-per-second))))
	(multiple-value-bind (mins secs)
	    (floor total-secs 60)
	  (format t " Run time: ~A minutes ~A seconds~%" mins secs))))
        
    ;; Describe parameters
    (describe-parameters-for-run max-gens pop-size seed)
    
    ;; Report on run
    (report-on-run)
    
    ))
