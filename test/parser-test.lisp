(in-package :de.srechenberger.parser-test)

(defvar *grammar-1*
  (list
   (list 'A (list 'B 'C))
   (list 'A (list :d :e))
   (list 'B (list 'C))
   (list 'B (list :eps))
   (list 'C (list :c))))

(deftest grammar-1-first-set-test ()
  (with-grammar *grammar-1*
    (check
      (equal (first-set 'A) (list :c :d))
      (equal (first-set 'B) (list :c :eps))
      (equal (first-set 'C) (list :c)))))

(deftest run-test ()
  (combine-results
    (grammar-1-first-set-test)))
