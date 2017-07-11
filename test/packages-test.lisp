(in-package :cl-user)

(defpackage :de.srechenberger.cl-parser-gen.parser-test
  (:nicknames :parser-test)
  (:use :cl
	:com.gigamonkeys.test
	:de.srechenberger.cl-parser-gen.parser)
  (:export :parser-test))

(defpackage :de.srechenberger.cl-parser-gen.tokenizer-test
  (:nicknames :tokenizer-test)
  (:use :cl
	:de.srechenberger.cl-parser-gen.tokenizer
	:com.gigamonkeys.test)
  (:export :tokenizer-test))

(defpackage :de.srechenberger.cl-parser-gen.test
  (:nicknames :test)
  (:use :cl
	:de.srechenberger.cl-parser-gen.parser-test
	:de.srechenberger.cl-parser-gen.tokenizer-test
	:com.gigamonkeys.test)
  (:export :test-all))
