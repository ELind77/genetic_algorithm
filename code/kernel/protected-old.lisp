;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  protected-functions.lisp
;;; ========================================
;;; Contains the protected functions for the function set
;;; of the kernel.
;;; These functions are designed to mimic their traditional
;;; mathematical counterparts, but without throwing errors
;;; that will stop evaluation.


;;;************************************************************
;;;  PROTECTED FUNCTIONS
;;;************************************************************

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
      (if (= 0 denominator)
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
    (if (< 10000000 result)
	10000000
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
    ;; Else
    (log (abs x))))


;;;============================================================
;;; PRO-EXPT
;;;============================================================
;;; Protected Eponential function
;;; Takes a BASE and a POWER and returns BASE^POWER.  But if
;;; this would cause a FLOATING-POINT-OVERFLOW, instead, return
;;; 10000000
;;; Code taken from franz (alisp) documentation:
;;; http://www.franz.com/support/documentation/8.2/doc/implementation.htm#infinities-nans-2

(defun pro-expt (base power)
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
		    (expt base power)))))
    ;; Filter
    (if (< 10000000 result)
	10000000
      ;; else
      result)))
		
		
;;;============================================================
;;; PRO-EXP
;;;============================================================
;;; Protected Eponential function (E^POWER)
;;; This is written in the same way as the PRO-EXPT function
;;; and behaves in much the same way.  The only difference is 
;;; that it only takes one argument.

(defun pro-exp (power)
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
    (if (< 10000000 result)
	10000000
      ;; else
      result)))