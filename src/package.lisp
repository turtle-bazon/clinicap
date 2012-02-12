;;;; -*- mode: lisp -*-

(defpackage :ru.bazon.clinicap
  (:nicknames :clinicap)
  (:use :cl)
  (:export
   
   :path-not-found
   :path-not-found-ini
   :path-not-found-path
   
   :ini
   :make-ini
   :ini-sections
   :ini-properties
   :get-ini-section
   :get-ini-property
   :set-ini-property
   
   :read-ini
   :read-ini-file
   :write-ini
   :write-ini-file))
