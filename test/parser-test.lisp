(in-package :de.srechenberger.parser-test)

(defvar *grammar-1*
  (list
   (list 'A (list 'B 'C))
   (list 'A (list :d :e))
   (list 'B (list 'C))
   (list 'B (list :eps))
   (list 'C (list :c))))

(define-grammar +grammar-1+
  ('A --> 'B 'C)
  ('A --> :d :e)
  ('B --> 'C)
  ('B --> :eps)
  ('C --> :c))

(defvar *grammar-2*
  (list
   (list :expr (list :sum))
   (list :sum  (list :product))
   (list :sum  (list :product #\+ :sum))
   (list :sum  (list :product #\- :sum))
   (list :product (list :value))
   (list :product (list :value #\+ :product))
   (list :product (list :value #\- :product))
   (list :value (list :id))
   (list :value (list #\( :expr #\)))))

(define-grammar +grammar-2+
  (:expr    --> :sum)
  (:sum     --> :product)
  (:sum     --> :product #\+ :sum)
  (:sum     --> :product #\- :sum)
  (:product --> :value)
  (:product --> :value #\+ :product)
  (:product --> :value #\- :product)
  (:value   --> :id)
  (:value   --> #\( :expr #\)))

(deftest def-grammar-test ()
  (check
    (equal +grammar-1+ *grammar-1*)
    (equal +grammar-2+ *grammar-2*)))

(deftest grammar-1-first-set-test ()
  (with-grammar *grammar-1*
    (check
      (equal (first-set 'A) (list :c :d))
      (equal (first-set 'B) (list :c :eps))
      (equal (first-set 'C) (list :c)))))

(deftest grammar-2-first-set-test ()
  (with-grammar *grammar-2*
    (check
      (equal (first-set :expr) (list :id #\())
      (equal (first-set :sum)  (list :id #\())
      (equal (first-set :product) (list :id #\())
      (equal (first-set :value) (list :id #\()))))

(deftest run-test ()
  (combine-results
    (def-grammar-test)
    (grammar-1-first-set-test)
    (grammar-2-first-set-test)))
