;;;; -*- mode: lisp -*-

(defpackage :ru.bazon.clinicap
  (:nicknames :clinicap)
  (:use :cl
	:iterate)
  (:export
   
   :path-not-found
   :path-not-found-ini
   :path-not-found-path
   
   :ini
   :make-ini
   :ini-name
   :ini-sections
   :ini-properties
   :add-ini-section
   :get-ini-section
   :get-ini-property
   :set-ini-property
   
   :read-ini
   :read-ini-file
   :write-ini
   :write-ini-file))
