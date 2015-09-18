;;; =========================
;;;  CMPU-365, Spring 2013
;;;  asmt-helper.lisp
;;; =========================
;;;  Some Helpful Functions when doing assignments.


;;;  TESTER
;;; ----------------------------
;;;  INPUT:  EXPR, anything
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Displays EXPR both before and after evaluation
;;; ----------------------------
;;;  Best used when the evaluation of EXPR does not cause any
;;;  side-effect printing.  Note, when calling this function
;;;  you should quote the expression of interest.  For example,
;;;  compare (tester (+ 1 2)) and (tester '(+ 1 2)) in the
;;;  Interactions Window.

(defun tester (expr)
  ;; NOTE:  FORMAT expressions evaluate to NIL
  (format t "~A ~%===> ~A~%~%" expr (eval expr)))


;;;  FANCY-TESTER
;;; ----------------------------
;;;  Same as TESTER, except that it displays any side-effect
;;;  printing caused by the evaluation of EXPR.  For example,
;;;  compare (tester '(format t "hi")) and (fancy-tester '(format t "hi"))
;;;  in the Interactions Window.

(defun fancy-tester (expr)
  ;; First, display EXPR (unevaluated)
  (format t "Expression: ~A~%" expr)
  (format t "----------------------~%")
  (let (;; Evaluating EXPR may cause side-effect printing HERE!
	(result (eval expr))) 
    (format t "~%----------------------~%")
    ;; After side-effect printing done, we now display the RESULT
    ;; (i.e., output value) of evaluating EXPR
    (format t "===> ~A~%~%" result))
  ;; NOTE:  FORMAT returns NIL
  )


;;;  PRINT-ASMT-HEADER
;;; ------------------------------
;;;  INPUTS:  NAME, your name (a string or symbol)
;;;           NUM, the assignment number (an integer)
;;;  OUTPUT:  nil
;;;  SIDE EFFECT: Displays a nice header in the "interactions window".

(defun print-asmt-header (name num)
  (format t "====================================~%")
  (format t "  CMPU-365, Spring 2013~%")
  (format t "  Asmt. ~A~%" num)
  (format t "  ~A~%" name)
  (format t "====================================~%~%"))

;;;  PRINT-PROBLEM-HEADER
;;; ---------------------------------
;;;  INPUT:  INFO, some information about an assignment problem
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Prints a nice header for an assignment problem
;;;   in the interactions window.

(defun print-problem-header (info)
  (format t "-------------------------~%")
  (format t " PROBLEM ~A~%" info)
  (format t "-------------------------~%~%"))


;;;  PRINT-TEST-HEADER
;;; ---------------------------------
;;;  INPUT:  NAME, Test name
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Prints a nice header for a test
;;;                in the interactions window.

(defun print-test-header (name)
  (format t "-------------------------~%")
  (format t " TESTS FOR ~A~%" name)
  (format t "-------------------------~%~%"))


;;;  PRINT-SECTION-HEADER
;;; ---------------------------------
;;;  INPUT:   NAME, section name
;;;  OUTPUT:  NIL
;;;  SIDE EFFECT:  Prints a nice header for a section
;;;                in the interactions window.

(defun print-section-header (name)
  (format t "~%*****************************************************~%")
  (format t "                 ~%~A~%" name)
  (format t "~%*****************************************************~%~%"))
