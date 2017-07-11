(in-package :de.srechenberger.all-test)

(deftest test-all ()
  (combine-results
    (de.srechenberger.tokenizer-test:run-test)
    (de.srechenberger.parser-test:run-test)))
