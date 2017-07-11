(in-package :de.srechenberger.cl-parser-gen.test)

(deftest test-all ()
  (combine-results
    (tokenizer-test:tokenizer-test)
    (parser-test:parser-test)))
