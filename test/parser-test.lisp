(in-package :de.srechenberger.cl-parser-gen.parser-test)

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

(define-case-test (first-set-test () :all-test-name first-set-tests)
  (*grammar-1*
   (check
     (equal (gethash 'A (first-sets *grammar-1*)) (list :c :d))
     (equal (gethash 'B (first-sets *grammar-1*)) (list :c :eps))
     (equal (gethash 'C (first-sets *grammar-1*)) (list :c))))
  (*grammar-2*
   (check
     (equal (gethash :expr (first-sets *grammar-2*)) (list :id #\())
     (equal (gethash :sum (first-sets *grammar-2*)) (list :id #\())
     (equal (gethash :product (first-sets *grammar-2*)) (list :id #\())
     (equal (gethash :value (first-sets *grammar-2*)) (list :id #\())))
  (*grammar-3*
   (check
     (equal (gethash :a (first-sets *grammar-3*)) (list :eps))))
  (*grammar-4*
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
  (*grammar-5*
   (let ((f (gethash :f (first-sets *grammar-5*))))
     (check
       (not (functionp (first f)))
       (not (functionp (second f)))
       (funcall (eval (first f)) 1)
       (funcall (eval (second f)) -1)))))

(define-test-for-all end-of-input-tests
    (grammar (*grammar-1* *grammar-2* *grammar-3* *grammar-4* *grammar-5*))
    ()
  (check
    (equal (gethash (start-symbol grammar)
		    (follow-sets grammar))
	   (list :$))))

(define-test parser-test ()
  (combine-results
    (def-grammar-test)
    (first-set-tests)
    (end-of-input-tests)))
