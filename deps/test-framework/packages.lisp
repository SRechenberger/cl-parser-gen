(in-package :cl-user)

(defpackage :com.gigamonkeys.test
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
  (:export :define-test
	   :check
	   :combine-results
	   :define-case-test
	   :define-test-for-all
	   :print-failed-tests
	   :*test-output*
	   :*test-cnt*
	   :*failed-cnt*
	   :*passed-cnt*
	   :red
	   :green))
