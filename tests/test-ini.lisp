;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-ini () ())

(addtest
    test-ini-get-property
  (ensure-same 1 1))
