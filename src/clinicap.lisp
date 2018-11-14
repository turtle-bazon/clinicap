;;;; -*- mode: lisp -*-

(in-package :ru.bazon.clinicap)

(defparameter *comment-chars* '(#\; #\#))

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
    (setf (ini-sections ini)
	  (reverse (cons new-section (ini-sections ini))))
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

(defun clean-comments (line)
  (coerce
   (iter (with state = 'TEXT)
	 (for char in-string line)
	 (when (eq char #\")
	   (if (eq state 'INSTRING)
	       (setf state 'TEXT)
	       (setf state 'INSTRING)))
	 (when (and (find char *comment-chars*)
		    (not (eq state 'INSTRING)))
	   (setf state 'COMMENT))
	 (when (not (eq state 'COMMENT))
	   (collect char)))
   'string))

(defun string-starts-with-digit (string)
  (let ((first-char (aref string 0)))
    (and (char>= first-char #\0) (char<= first-char #\9))))

(defun string-starts-with (string fragment)
  (eq 0 (search fragment string)))

(defun string-ends-with (string fragment)
  (eq (- (length string) (length fragment))
      (search fragment string :from-end t)))

(defun parse-section (line)
  (let ((section-name (subseq line 1 (- (length line) 1))))
    `(:section ,section-name)))

(defun parse-number (string)
  (let ((dot-index (search "." string)))
    (cond (dot-index
           (let* ((integer-part (parse-integer (subseq string 0 dot-index)))
                  (real-part-raw (subseq string (+ dot-index 1)))
                  (real-part (parse-integer real-part-raw))
                  (real-length (length real-part-raw)))
             (coerce
              (+ integer-part
                 (/ real-part
                    (expt 10 real-length))) 'float)))
          ((string-starts-with string "0x")
           (parse-integer string :start 2 :radix 16))
          ((string-starts-with string "0")
           (parse-integer string :start 1 :radix 8))
          (t (parse-integer string)))))

(defun parse-value (line)
  (let* ((eq-index (search "=" line))
         (name (string-trim (list #\space #\tab) (subseq line 0 eq-index)))
         (raw-value (string-trim (list #\space #\tab) (subseq line (+ eq-index 1)))))
    `(:property ,name ,(cond ((and (string-starts-with raw-value "\"")
                                   (string-ends-with raw-value "\""))
                              (subseq raw-value 1 (- (length raw-value) 1)))
                             ((string-starts-with-digit raw-value)
                              (parse-number raw-value))
                             (t (intern raw-value))))))

(defun parse-ini-line-def (line)
  (let ((clean-line (string-trim (list #\space #\return #\tab) (clean-comments line))))
    (cond
      ((equal clean-line "") nil)
      ((and (eq #\[ (aref clean-line 0))
	    (eq #\] (aref clean-line (- (length clean-line) 1))))
       (parse-section clean-line))
      ((find #\= clean-line) (parse-value clean-line))
      (t (error "Unrecognized line: ~a" line)))))

(defun read-ini (stream)
  (let ((root-ini (make-ini :name "ROOT")))
    (iter (with ini = root-ini)
	  (for line = (read-line stream nil nil))
	  (while (not (eq line nil)))
	  (let ((parsed-line (parse-ini-line-def line)))
	    (when parsed-line
	      (ecase (first parsed-line)
		(:section (setf ini
				(add-ini-section root-ini
						 (second parsed-line))))
		(:property (set-ini-property ini
					     `(,(second parsed-line))
					     (third parsed-line)))))))
    root-ini))

(defun read-ini-file (file-spec &optional &key (encoding :utf-8))
  (with-open-file (stream file-spec
			  :direction :input
			  :element-type 'character
			  :external-format encoding)
    (read-ini stream)))

(defun write-ini (stream ini)
  (iter (for (key . value) in (reverse (ini-properties ini)))
	(format stream "~a=~:[~a~;\"~a\"~]~%" key (stringp value) value))
  (when (or (not (equal (ini-name ini) "ROOT"))
	    (ini-properties ini))
    (format stream "~%"))
  (iter (for section in (ini-sections ini))
	(format stream "[~a]~%" (ini-name section))
	(write-ini stream section)))

(defun write-ini-file (file-spec ini &optional &key (encoding :utf-8))
  (with-open-file (stream file-spec
			  :direction :output
			  :element-type 'character
			  :external-format encoding
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (write-ini stream ini)))
