(in-package :cl-user)

(defpackage :de.srechenberger.parser-test
  (:use :cl
	:com.gigamonkeys.test
	:de.srechenberger.parser)
  (:export :run-test))

(defpackage :de.srechenberger.tokenizer-test
  (:use :cl
	:de.srechenberger.tokenizer
	:com.gigamonkeys.test)
  (:export :run-test))

(defpackage :de.srechenberger.all-test
  (:use :cl
	:com.gigamonkeys.test)
  (:export :test-all))
