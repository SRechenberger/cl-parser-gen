(defpackage :de.srechenberger.parser-gen-test-system
  (:use :asdf :cl))

(in-package :de.srechenberger.parser-gen-test-system)

(defsystem :cl-parser-gen-test
  :name "cl-parser-gen-test"
  :author "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
  :version "0.1"
  :maintainer "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
  :license "BSD"
  :description "Parser generatoin utilities test suite"
  :components ((:file "packages-test")
	       (:file "tokenizer-test" :depends-on ("packages-test"))
	       (:file "parser-test" :depends-on ("packages-test"))
	       (:file "all-test" :depends-on ("tokenizer-test" "parser-test")))
  :depends-on (:cl-parser-gen
	       :macro-utilities
	       :test-framework))
