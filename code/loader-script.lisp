;;; ========================================
;;; CMPU-365, Spring 2013
;;; Final Project: Genetic Coding
;;; Name: Eric Lind
;;; ========================================
;;; FILE:  loader-script.lisp
;;; ========================================

;; This is just a simple script to compile and load the 
;; needed files


;;;=====================================================
;;; LOADER
;;;=====================================================
;;; INPUTS:   File names
;;; OUTPUT:   NONE
;;; Side FX:  Comples and loads files for the current 
;;            assignment.

(defun loader (file1 &optional (file2 nil) (file3 nil) (file4 nil))

	;; Compiler switches
  	;;(setq compiler:tail-call-self-merge-switch t)
	;;(setq compiler:tail-call-non-self-merge-switch t)
        
	;; Helper files
	(compile-file "asmt-helper")
	(load "asmt-helper")
	
	;; FILE1
	(compile-file file1)
	(load file1)
	
	;; Additional Files
	(dolist (i (list file2 file3 file4))
	  ;; If the current file is not NIL compile and load it
	  (if (not (null i))
	      (progn
		(compile-file i)
		(load i))))
	
	
	;;(loader)
	(format t "Files are all loaded. We're good to go.~%"))

