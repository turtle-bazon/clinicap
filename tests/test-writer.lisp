;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-writer () ())

(addtest
    test-write-config
  (ensure-same 1 1))
