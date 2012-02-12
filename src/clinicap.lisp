;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap)

(define-condition path-not-found (error)
  ((ini :initarg :ini
	:reader path-not-found-ini
	:initform (error ":ini must be defined"))
   (path :initarg :path
	 :reader path-not-found-path
	 :initform (error ":path must be defined"))))

(defstruct ini
  (name nil :type string)
  (sections nil :type list)
  (properties nil :type list))

(defmacro get-property (properties name)
  `(cdr (assoc ,name ,properties :test #'equal)))

(defmacro set-property (properties name value)
  (let ((cons (gensym "CONS")))
    `(let ((,cons (assoc ,name ,properties :test #'equal)))
       (if ,cons
	   (rplacd ,cons ,value)
	   (push (cons ,name ,value) ,properties)))))

(defun get-ini-section (ini name)
  (find name (ini-sections ini) :test #'equal :key #'ini-name))

(defun add-ini-section (ini name)
  (let ((new-section (make-ini :name name)))
    (push new-section (ini-sections ini))
    new-section))

(defun get-ini-property (ini path)
  (case (length path)
    (0 (error (make-condition 'path-not-found :ini ini :path path)))
    (1 (get-property (ini-properties ini) (first path)))
    (t (let ((section (get-ini-section ini (first path))))
	 (if (not section)
	     nil
	     (get-ini-property section (rest path)))))))

(defun set-ini-property (ini path value)
  (case (length path)
    (0 (error (make-condition 'path-not-found :ini ini :path path)))
    (1 (set-property (ini-properties ini) (first path) value))
    (t (let* ((section-name (first path))
	      (rest-path (rest path))
	      (section (get-ini-section ini section-name)))
	 (if (not section)
	     (set-ini-property (add-ini-section ini section-name) rest-path value)
	     (set-ini-property section rest-path value))))))

(defun read-ini (stream &optional &key (encoding :utf-8))
  )

(defun read-ini-file (file-spec &optional &key (encoding :utf-8))
  (with-open-file (stream file-spec
			  :direction :input
			  :element-type 'character)
    (read-ini stream :encoding encoding)))

(defun write-ini (stream ini &optional &key (encoding :utf-8))
  )

(defun write-ini-file (file-spec ini &optional &key (encoding :utf-8))
  (with-open-file (stream file-spec
			  :direction :output
			  :element-type 'character
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (write-ini stream ini :encoding encoding)))
