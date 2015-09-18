;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  protected-functions-2.lisp
;;; ========================================
;;; Contains the protected functions for the function set
;;; of the kernel.
;;; These functions are designed to mimic their traditional
;;; mathematical counterparts, but without throwing errors
;;; that will stop evaluation.


;;;************************************************************
;;;  PROTECTED FUNCTIONS
;;;************************************************************

;;; Functions in this file
;;;============================
;; %     -- division
;; srt   -- square root
;; rsin  -- sine
;; rsinh -- hyperbolic sine
;; rtan
;; ratan
;; rlog  -- natural logarithm
;; rexpt -- exponential
;; rexp  -- exponential



;;;============================================================
;;; %
;;;============================================================
;;; The protected division function. Returns 1 if user attempts
;;; to divide by zero.

(defun % (numerator denominator)
  "The protected division function"
  (catch 'trap-error
    (handler-bind (
		   ;; If FLOATING-POINT-OVERFLOW
		   ;; Return 10000000
		   (floating-point-overflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 10000000)))
		   ;; If FLOATING-POINT-UNDERFLOW
		   ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		   (floating-point-underflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error least-positive-single-float))))
      ;; Expression   
      (if (equalp 0 denominator)
	  1
	(/ numerator denominator)))))


;;;============================================================
;;; SRT
;;;============================================================
;;; The protected square-root function. Returns the square root
;;; of the absolute value of the input.
;;;
;;; NOTE: If for some reason, and this seems to happen far more
;;;       often than it should, SRT get one of the Not-A-NUMBER
;;;       constants (e.g. *NAN-SINGLE*) as an argument it will 
;;;       return 1.0.

(defun srt (x)
  (catch 'trap-error
    (handler-bind (
		   ;; If FLOATING-POINT-OVERFLOW
		   ;; Return 10000000
		   (floating-point-overflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 10000000)))
		   ;; If FLOATING-POINT-UNDERFLOW
		   ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		   (floating-point-underflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error least-positive-single-float)))
		   ;; Case to handle *NAN-SINGLE* and *NAN-DOUBLE*
		   (simple-error
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 1.0))))
      ;; Expression
      (sqrt (abs x)))))


;;;============================================================
;;; RSIN
;;;============================================================
;;; The protected sine function

(defun rsin (x)
   (catch 'trap-error
    (handler-bind (
		   ;; If FLOATING-POINT-OVERFLOW
		   ;; Return 10000000
		   (floating-point-overflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 10000000)))
		   ;; If FLOATING-POINT-UNDERFLOW
		   ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		   (floating-point-underflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error least-positive-single-float))))
      ;; Expression
      (sin x))))


;;;============================================================
;;; RSINH
;;;============================================================
;;; The protected hyperbolic-sine function

(defun rsinh (x)
  (let ((result (catch 'trap-error
		  (handler-bind
		      ;; If FLOATING-POINT-OVERFLOW
		      ;; Return 10000000
		      ((floating-point-overflow
			#'(lambda (c)
			    (declare (ignore c))
			    (throw 'trap-error 10000000)))
		       ;; If FLOATING-POINT-UNDERFLOW
		       ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		       (floating-point-underflow
			#'(lambda (c)
			    (declare (ignore c))
			    (throw 'trap-error least-positive-single-float))))
		    ;; Expression
		    (sinh x)))))
    ;; Filter 
    (if (< 1000000 (abs result))
	(* (signum result) 10000000)
      ;; Else
      result)))
	

;;;============================================================
;;; RTAN
;;;============================================================
;;; The protected tangent function

(defun rtan (x)
   (catch 'trap-error
    (handler-bind (
		   ;; If FLOATING-POINT-OVERFLOW
		   ;; Return 10000000
		   (floating-point-overflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 10000000)))
		   ;; If FLOATING-POINT-UNDERFLOW
		   ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		   (floating-point-underflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error least-positive-single-float))))
      ;; Expression
      (tan x))))


;;;============================================================
;;; RATAN
;;;============================================================
;;; The protected hyperbolic arc-tangent function

(defun ratan (x)
   (catch 'trap-error
    (handler-bind (
		   ;; If FLOATING-POINT-OVERFLOW
		   ;; Return 10000000
		   (floating-point-overflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error 10000000)))
		   ;; If FLOATING-POINT-UNDERFLOW
		   ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		   (floating-point-underflow
		    #'(lambda (c)
			(declare (ignore c))
			(throw 'trap-error least-positive-single-float))))
      ;; Expression
      (atan x))))


;;;============================================================
;;; RLOG
;;;============================================================
;;; The protected natural-logarithm function.

(defun rlog (x)
  (if (= 0 x)
      0
    ;; else
    (log (abs x))))



;;;============================================================
;;; REXPT
;;;============================================================
;;; Protected Eponential function
;;; Takes a BASE and a POWER and returns BASE^POWER.  But if
;;; this would cause a FLOATING-POINT-OVERFLOW, instead, return
;;; 10000000
;;; Code taken from franz (alisp) documentation:
;;; http://www.franz.com/support/documentation/8.2/doc/implementation.htm#infinities-nans-2

(defun rexpt (base power)
  ;; This isn't a very good way to do this, but I also want 
  ;; to limit the possible outputs that this could give,
  ;; even if it doesn't throw an error so I'm filtering the
  ;; output a bit.
  (let ((result (catch 'trap-error
		  (handler-bind 
		      ;; If FLOATING-POINT-OVERFLOW
		      ;; Return 10000000
		      ((floating-point-overflow
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 10000000)))
		       ;; If FLOATING-POINT-UNDERFLOW
		       ;; Return: 0
		       (floating-point-underflow
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 
			      0)))
		       ;; Because of the BIGNUM limits in ALISP
		       ;; EXPT will also occasionally throw a simple
		       ;; error (Integer too large to represent). 
		       ;; Although catching a simple error isn't the best
		       ;; way to deal with that it works here.
		       (simple-error
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 
			       10000000)))
		       ;; If the BASE is small enough and put to a 
		       ;; negative power, EXPT will throw a 
		       ;; DIVISION-BY-ZERO error
		       (division-by-zero
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 
			       10000000))))
		    ;; Expression
		    ;; The EXPT function is bugged!
		    ;; If the second option given is 0.0 it
		    ;; will throw a type error, so that has
		    ;; to be checked for too.  And we need 
		    ;; to check fo a BASE value of zero.
		    (cond
		     ;; 0.0 problem
		     ((equalp 0.0 power)
		      1)
		     ;; Zero with a negative POWER
		     ((and (equalp base 0) (< power 0))
		      0)
		     ;; Otherwise:
		     ;; Also handles imaginary output.
		     ;; We don't want non-reals, but I thought
		     ;; preserving the real part might result in
		     ;; more interesting results.
		     (t
		      (realpart (expt base power))))))))
    ;; Filter
    (cond
     ;; *INFINITY-SINGLE*
     ((equalp *infinity-single* result)
      10000000)
     ;; *NEGATIVE-INFINITY-SINGLE*
     ((equalp *negative-infinity-single* result)
      -10000000)
     ;; Other large results too big
     ((< 10000000 (abs result))
      (* (signum result) 10000000))
     ;; else
     (t
      result))))


;;;============================================================
;;; REXP
;;;============================================================
;;; Protected Eponential function (E^POWER)
;;; This is written in the same way as the PRO-EXPT function
;;; and behaves in much the same way.  The only difference is 
;;; that it only takes one argument.

(defun rexp (power)
  ;; This isn't a very good way to do this, but I also want 
  ;; to limit the possible outputs that this could give,
  ;; even if it doesn't throw an error so I'm filtering the
  ;; output a bit.
  (let ((result (catch 'trap-error
		  (handler-bind 
		      ;; If FLOATING-POINT-OVERFLOW
		      ;; Return 10000000
		      ((floating-point-overflow
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 10000000)))
		       ;; If FLOATING-POINT-UNDERFLOW
		       ;; Return LEAST-POSITIVE-SINGLE-FLOAT
		       (floating-point-underflow
			#'(lambda (c)
			    (declare (ignore c))
			     (throw 'trap-error 
			       least-positive-single-float))))
		    ;; Expression
		    (exp power)))))
    
;; Filter
(cond
     ;; *INFINITY-SINGLE*
     ((equalp *infinity-single* result)
      10000000)
     ;; *NEGATIVE-INFINITY-SINGLE*
     ((equalp *negative-infinity-single* result)
      -10000000)
     ;; Other large results too big
     ((< 10000000 (abs result))
      (* (signum result) 10000000))
     ;; else
     (t
      result))))
