;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-basics () ())

(addtest
    test-read-config
  (ensure-same 1 2))
