;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-writer () ())

(addtest
    test-write-config
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("general" "max-connections") 20)
    (ensure-same
     "[general]
max-connections=20"
     (with-output-to-string (stream)
       (write-ini stream ini)))))
