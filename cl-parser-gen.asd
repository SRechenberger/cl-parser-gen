(defpackage :de.srechenberger.parser-gen-system
  (:use :asdf :cl))

(in-package :de.srechenberger.parser-gen-system)

(defsystem :cl-parser-gen
    :name "cl-parser-gen"
    :author "Sascha Rechenberger"
    :version "0.1"
    :maintainer "Sascha Rechenberger"
    :license "BSD"
    :description "Parser generation utilties"
    :components ((:file "packages")
		 (:file "tokenizer" :depends-on ("packages")))
    :depends-on (:macro-utilities
		 :cl-ppcre))
