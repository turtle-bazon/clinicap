;;;; -*- mode: lisp -*-

(defpackage :ru.bazon.clinicap-tests
  (:nicknames :clinicap-tests)
  (:use :cl
	:clinicap
	:lift
	:iterate
	:cl-match)
  (:export
   :run-all-tests))

(in-package :ru.bazon.clinicap-tests)

;;; 
;;; run-tests
;;;
(defun run-all-tests ()
  (let* ((test-results (iter (for test-suite in (testsuites))
			     (for test-name = (class-name test-suite))
			     (for test-suite-results = (run-tests :suite test-name
								  :report-pathname nil))
			     (collect
				 (list test-suite-results
				       (testsuite-tests test-name)
				       (failures test-suite-results)))))
	 (test-results-reduced (reduce
				#'(lambda (x y)
				    (list (+ (first x) (first y))
					  (+ (second x) (second y))))
				(iter (for (_ tests-run-local tests-failed-local) in test-results)
				      (collect (list (length tests-run-local)
						     (length tests-failed-local)))))))
    (format t "~%~a~%" test-results-reduced)
    (multiple-value-bind (a b)
	test-results-reduced
      (format t "~a,~a~%" a b))
    (multiple-value-bind (tests-run tests-failed)
	test-results-reduced
      (format t "~%~%")
      (format t "r: ~a~%" tests-run)
      (format t "f: ~a~%" tests-failed)
      (format t "~a~%" (reduce
			#'(lambda (x y)
			    (list (+ (first x) (first y))
				  (+ (second x) (second y))))
			(iter (for (_ tests-run-local tests-failed-local) in test-results)
			      (collect (list (length tests-run-local)
					     (length tests-failed-local))))))
      #+nil(iter (for test-result in test-results)
		 (describe-test-result test-result t))
      (list tests-run tests-failed)
      #+nil(format t "~%Test Report for all tests: ~a tests run, ~a!"
		   tests-run
		   (if (> tests-failed 0)
		       (format nil "~a tests failed" tests-failed)
		       "all passed")))
    #+nil(match (reduce
		 #'(lambda (x y)
		     (list (+ (first x) (first y))
			   (+ (second x) (second y))))
		 (iter (for test-result in test-results)
		       (collect
			   (match test-result
			     ((list _ tests-run-local tests-failed-local)
			      (list (length tests-run-local)
				    (length tests-failed-local)))))))
	   ((list tests-run tests-failed)
	    (format t "~%~%")
	    (iter (for test-result in test-results)
		  (describe-test-result test-result t))
	    (format t "~%Test Report for all tests: ~a tests run, ~a!"
		    tests-run
		    (if (> tests-failed 0)
			(format nil "~a tests failed" tests-failed)
			"all passed"))))))
