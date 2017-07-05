(defpackage :de.srechenberger.parser-gen-system
  (:use :asdf :cl))

(in-package :de.srechenberger.parser-gen-system)

(defsystem :cl-parser-gen
    :name "cl-parser-gen"
    :author "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
    :version "0.1"
    :maintainer "Sascha Rechenberger <sascha.rechenberger@uni-ulm.de>"
    :license "BSD"
    :description "Parser generation utilties"
    :components ((:file "packages")
		 (:file "tokenizer" :depends-on ("packages"))
		 (:file "examples" :depends-on ("tokenizer")))
    :depends-on (:macro-utilities
		 :test-framework
		 :cl-ppcre))
