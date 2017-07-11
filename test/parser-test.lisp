(in-package :de.srechenberger.cl-parser-gen.parser-test)

(defparameter *grammar-1-ctrl*
  (list
   (list 'A (list 'B 'C))
   (list 'A (list :d :e))
   (list 'B (list 'C))
   (list 'B (list :eps))
   (list 'C (list :c))))

(define-grammar *grammar-1*
  ('A --> 'B 'C)
  ('A --> :d :e)
  ('B --> 'C)
  ('B --> :eps)
  ('C --> :c))

(defparameter *grammar-2-ctrl*
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

(define-grammar *grammar-2*
  (:expr    --> :sum)
  (:sum     --> :product)
  (:sum     --> :product #\+ :sum)
  (:sum     --> :product #\- :sum)
  (:product --> :value)
  (:product --> :value #\+ :product)
  (:product --> :value #\- :product)
  (:value   --> :id)
  (:value   --> #\( :expr #\)))

(defparameter *grammar-3-ctrl*
  (list (list :a (list))))

(define-grammar *grammar-3*
  (:a -->))

(deftest def-grammar-test ()
  (check
    (equal *grammar-1* *grammar-1-ctrl*)
    (equal *grammar-2* *grammar-2-ctrl*)
    (equal *grammar-3* *grammar-3-ctrl*)))

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

(deftest grammar-3-first-set-test ()
  (with-grammar *grammar-3*
    (check
      (equal (first-set :a) (list :eps)))))
	      
(deftest parser-test ()
  (combine-results
    (def-grammar-test)
    (grammar-1-first-set-test)
    (grammar-2-first-set-test)
    (grammar-3-first-set-test)))
