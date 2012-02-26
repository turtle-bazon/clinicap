;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-writer () ())

(addtest
    test-write-property-non-string
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("max-connections") 20)
    (ensure-same
     "max-connections=20

"
     (with-output-to-string (stream)
       (write-ini stream ini)))))

(addtest
    test-write-property-string
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("e-mail") "e-mail@example.com")
    (ensure-same
     "e-mail=\"e-mail@example.com\"

"
     (with-output-to-string (stream)
       (write-ini stream ini)))))

(addtest
    test-write-two-properties
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("max-connections") 20)
    (set-ini-property ini '("e-mail") "e-mail@example.com")
    (ensure-same
     "max-connections=20
e-mail=\"e-mail@example.com\"

"
     (with-output-to-string (stream)
       (write-ini stream ini)))))

(addtest
    test-write-property-in-section
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("general" "max-connections") 20)
    (ensure-same
     "[general]
max-connections=20

"
     (with-output-to-string (stream)
       (write-ini stream ini)))))

(addtest
    test-write-properties-and-sections
  (let ((ini (make-ini :name "ROOT")))
    (set-ini-property ini '("default-max-connections") 10)
    (set-ini-property ini '("default-reconnect-mode") 2)
    (set-ini-property ini '("general" "max-connections") 20)
    (set-ini-property ini '("general" "e-mail") "e-mail@example.com")
    (set-ini-property ini '("section1" "connect") t)
    (set-ini-property ini '("section1" "retry-count") 15)
    (ensure-same
     "default-max-connections=10
default-reconnect-mode=2

[general]
max-connections=20
e-mail=\"e-mail@example.com\"

[section1]
connect=T
retry-count=15

"
     (with-output-to-string (stream)
       (write-ini stream ini)))))
