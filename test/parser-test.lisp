(in-package :de.srechenberger.cl-parser-gen.parser-test)


(defmacro define-end-of-input-test (name grammar)
  `(define-test ,name ()
     (check
       (equal (gethash (start-symbol ,grammar)
		       (follow-sets ,grammar))
	      (list :$)))))

(defparameter *grammar-1-ctrl*
  (list
   (list 'A (list 'B 'C))
   (list 'A (list :d :e))
   (list 'B (list 'C))
   (list 'B (list :eps))
   (list 'C (list :c))))

(define-grammar *grammar-1* A
  (A --> B C)
  (A --> :d :e)
  (B --> C)
  (B --> :eps)
  (C --> :c))

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

(define-grammar *grammar-2* :expr
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

(define-grammar *grammar-3* :a
  (:a -->))

(define-grammar *grammar-4* :f
  (:f --> #'(lambda (x) (< 0 x)) :pos)
  (:f --> #'(lambda (x) (< x 0)) :neg)
  (:g --> #'evenp :even)
  (:g --> #'oddp :odd))

(define-grammar *grammar-5* :f
  (:f --> (lambda (x) (< 0 x)) :pos)
  (:f --> (lambda (x) (< x 0)) :neg))

(define-test def-grammar-test ()
  (check
    (equal (rules *grammar-1*) *grammar-1-ctrl*)
    (equal (rules *grammar-2*) *grammar-2-ctrl*)
    (equal (rules *grammar-3*) *grammar-3-ctrl*)))

(define-test grammar-1-first-set-test ()
  (check
   (equal (gethash 'A (first-sets *grammar-1*)) (list :c :d))
   (equal (gethash 'B (first-sets *grammar-1*)) (list :c :eps))
   (equal (gethash 'C (first-sets *grammar-1*)) (list :c))))



(define-test grammar-2-first-set-test ()
  (check
   (equal (gethash :expr (first-sets *grammar-2*)) (list :id #\())
   (equal (gethash :sum (first-sets *grammar-2*)) (list :id #\())
   (equal (gethash :product (first-sets *grammar-2*)) (list :id #\())
   (equal (gethash :value (first-sets *grammar-2*)) (list :id #\())))

(define-test grammar-3-first-set-test ()
  (check
    (equal (gethash :a (first-sets *grammar-3*)) (list :eps))))

(define-test grammar-4-first-set-test ()
  (let ((g (gethash :g (first-sets *grammar-4*)))
	(f (gethash :f (first-sets *grammar-4*))))
    (check
      (not (functionp (first g)))
      (not (functionp (second g)))
      (not (functionp (first f)))
      (not (functionp (second f)))
      (funcall (eval (first g)) 2)
      (funcall (eval (second g)) 3)
      (funcall (eval (first f)) 1)
      (funcall (eval (second g)) -1))))

(define-test grammar-5-first-set-test ()
  (let ((f (gethash :f (first-sets *grammar-5*))))
    (check
      (not (functionp (first f)))
      (not (functionp (second f)))
      (funcall (eval (first f)) 1)
      (funcall (eval (second f)) -1))))

(define-end-of-input-test grammar-1-eoi-test *grammar-1*)
(define-end-of-input-test grammar-2-eoi-test *grammar-2*)
(define-end-of-input-test grammar-3-eoi-test *grammar-3*)
(define-end-of-input-test grammar-4-eoi-test *grammar-4*)
(define-end-of-input-test grammar-5-eoi-test *grammar-5*)

(define-test grammar-1-tests ()
  (combine-results
    (grammar-1-first-set-test)
    (grammar-1-eoi-test)))

(define-test grammar-2-tests ()
  (combine-results
    (grammar-2-first-set-test)
    (grammar-2-eoi-test)))

(define-test grammar-3-tests ()
  (combine-results
    (grammar-3-first-set-test)
    (grammar-3-eoi-test)))

(define-test grammar-4-tests ()
  (combine-results
    (grammar-4-first-set-test)
    (grammar-4-eoi-test)))

(define-test grammar-5-tests ()
  (combine-results
    (grammar-5-first-set-test)
    (grammar-5-eoi-test)))

(define-test parser-test ()
  (combine-results
    (def-grammar-test)
    (grammar-1-tests)
    (grammar-2-tests)
    (grammar-3-tests)
    (grammar-4-tests)
    (grammar-5-tests)))
