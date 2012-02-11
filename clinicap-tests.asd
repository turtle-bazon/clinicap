;;;; -*- mode: lisp -*-

(defsystem :clinicap-tests
  :name "clinicap-tests"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :description "Common Lisp INI Composer And Parser"
  :depends-on (clinicap lift iterate cl-match)
  :components ((:module "tests"
			:components
			((:file "package")
			 (:file "test-basics" :depends-on ("package"))))))
