;;;; -*- mode: lisp -*-

(defsystem :clinicap
  :name "cliniicap"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Common Lisp INI Composer and Creator"
  :depends-on ()
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "clinicap" :depends-on ("package")))))
  :in-order-to ((test-op (test-op clinicap-tests)))
  :perform (test-op :after (op c)
		    (funcall
		     (intern (symbol-name '#:run-all-tests)
			     :clinicap-tests))))
