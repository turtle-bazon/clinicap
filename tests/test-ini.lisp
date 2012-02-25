;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap-tests)

(deftestsuite test-ini () ())

(addtest
    test-ini-defaults
  (let ((root-ini (make-ini :name "ROOT")))
    (ensure-same nil (ini-sections root-ini))
    (ensure-same nil (ini-properties root-ini))))

(addtest
    test-ini-section
  (let ((root-ini (make-ini :name "ROOT")))
    (add-ini-section root-ini "general")
    (ensure-same 1 (length (ini-sections root-ini)))
    (ensure-same "general" (ini-name (first (ini-sections root-ini))))))

(addtest
    test-ini-property
  (let ((root-ini (make-ini :name "ROOT")))
    (set-ini-property root-ini '("general" "max-connections") 20)
    (ensure-same 20 (get-ini-property root-ini '("general" "max-connections")))
    (ensure-same 1 (length (ini-sections root-ini)))
    (let ((general-section (first (ini-sections root-ini))))
      (ensure-same 1 (length (ini-properties general-section)))
      (ensure-same 20 (cdr (first (ini-properties general-section)))))))