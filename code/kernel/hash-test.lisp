;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Programing
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  hash-test.lisp
;;; ========================================
;;; File for testing hash table stuff


(defvar *date-table* (make-hash-table :test #'equal))



;;; MAKE-DATE-TABLE
;;;==================================================
;;; Should make a hash table of dates corresponding to line
;;; numbers in data.txt

(defun make-date-table ()
  (with-open-file (stream "date-range.txt")
    ;; Date-range.txt has 12898 records
    (dotimes (i 12897)
      (let ((line (read stream)))
	(setf (gethash (second line) *date-table*) (first line))))))