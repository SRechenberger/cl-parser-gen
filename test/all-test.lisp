(in-package :de.srechenberger.cl-parser-gen.test)

(define-test test-all ()
  (format *test-output* "~%Running tests...~%")
  (if (combine-results
       (tokenizer-test:tokenizer-test)
       (parser-test:parser-test))
      (progn
	(format *test-output* "~&~a. All ~d tests passed.~%" (green "Ok") *test-cnt*)
	t)
      (progn
	(format *test-output* "~&~a. ~d of ~d tests failed.~%" (red "Fail") *failed-cnt* *test-cnt*)
	(print-failed-tests *test-output*)
	t)))
