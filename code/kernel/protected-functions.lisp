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
      (if (equalp 0 denominator)
	  1
	(/ numerator denominator)))))

		
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
    (if (< 10000000 (abs result))
	(* (signum result) 10000000)
      ;; else
      result)))