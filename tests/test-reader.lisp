;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(defun read-ini-from-string (string)
  (with-input-from-string (s string)
    (read-ini s)))

(deftestsuite test-reader () ())

(addtest
    test-read-config
  (ensure-same t (typep (read-ini-from-string "") 'ini)))

(addtest
    test-read-config-only-properties
  (let ((ini (read-ini-from-string "p1=1
p2=2
p3=3")))
    (ensure-same 0 (length (ini-sections ini)))
    (ensure-same 3 (length (ini-properties ini)))))

(addtest
    test-read-config-empty-section
  (let ((ini (read-ini-from-string "[s1]
[s2]
s2p1=1
s2p2=2")))
    (ensure-same 2 (length (ini-sections ini)))
    (ensure-same 0 (length (ini-properties ini)))
    (ensure-same 2 (length (ini-properties (second (ini-sections ini)))))))

(addtest
    test-read-config-basic-types
  (let ((ini (read-ini-from-string "p1=1
p2=1.0
p3=\"data\"")))
    (destructuring-bind (p1 p2 p3)
	(ini-properties ini)
      )))