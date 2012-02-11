;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap)

(defun read-ini (stream &optional &key (encoding :utf-8))
  )

(defun read-ini-file (file-spec &optional &key (encoding :utf-8))
  )

(defun write-ini (stream ini &optional &key (encoding :utf-8))
  )

(defun write-ini-file (file-spec ini &optional &key (encoding :utf-8))
  (with-open-file (stream file-spec
			  :direction :output
			  :element-type 'character
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (write-ini stream ini)))
