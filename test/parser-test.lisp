(in-package :de.srechenberger.cl-parser-gen.parser-test)

;;; Utilities 
(defun set-equal (set-1 set-2)
  "Checks, wether two sets are equal."
  (and (subsetp set-1 set-2)
       (subsetp set-2 set-1)))
   
(defmacro define-accept-test (name parser parameters &body inputs)
  "Defines a test with a given `NAME`, which checks, wether a `PARSER` accepts all given `INPUTS`"
  `(define-test ,name ,parameters
    (check
      ,@(mapcar #'(lambda (input) `(,parser ,input)) inputs))))

(defmacro define-reject-test (name parser parameters &body inputs)
  "Defines a test with a given `NAME`, which checks, wether a `PARSER` rejects all given `INPUTS`"
  `(define-test ,name ,parameters
     (check
       ,@(mapcar #'(lambda (input) `(not (,parser ,input))) inputs))))

(defun has-first-set (symb set grammar)
  "Checks, wether a `GRAMMAR` has the given `SET` as first set for `SYMB`"
  `(set-equal (gethash ',symb (first-sets ,grammar)) ,set))

(defmacro check-first-sets (grammar &body body)
  "Checks all given first sets of `GRAMMAR`"
  `(check
     ,@(loop
	  for (symb set) in body
	  collect `(set-equal (gethash ,symb (first-sets ,grammar)) ,set))))

(defun has-follow-set (symb set grammar)
  "Checks, wether a `GRAMMAR` has the given `SET` as follow set for `SYMB`"
  `(set-equal (gethash ',symb (follow-sets ,grammar)) ,set))

(defmacro check-follow-sets (grammar &body body)
  "Checks all given follow sets of `GRAMMAR`"
  `(check
     ,@(loop
	  for (symb set) in body
	  collect `(set-equal (gethash ,symb (follow-sets ,grammar)) ,set))))

;;;; TESTS

;;; Grammar 1-8 
;;; Testing structure
(define-grammar *grammar-1* A
  (A --> B C)
  (A --> :d :e)
  (B --> C)
  (B --> :eps)
  (C --> :c))

(defparameter *grammar-1-ctrl*
  (list
   (list 'A (list 'B 'C))
   (list 'A (list :d :e))
   (list 'B (list 'C))
   (list 'B (list :eps))
   (list 'C (list :c))))


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


(define-grammar *grammar-6* :E
  (:E --> :T :X)
  (:X --> #\+ :T :X)
  (:X --> :eps)
  (:T --> :F :Y)
  (:Y --> #\* :F :Y)
  (:Y --> :eps)
  (:F --> #\( :E #\))
  (:F --> :id))

(defparameter *grammar-6-ctrl*
  (list
   (list :E (list :T :X))
   (list :X (list #\+ :T :X))
   (list :X (list :eps))
   (list :T (list :F :Y))
   (list :Y (list #\* :F :Y))
   (list :Y (list :eps))
   (list :F (list #\( :E #\)))
   (list :F (list :id))))

(define-test def-grammar-test ()
  (check
    (equal (rules *grammar-1*) *grammar-1-ctrl*)
    (equal (rules *grammar-2*) *grammar-2-ctrl*)
    (equal (rules *grammar-3*) *grammar-3-ctrl*)
    (equal (rules *grammar-6*) *grammar-6-ctrl*)))

(define-grammar *grammar-7* :S
  (:S --> :A)
  (:A --> :B)
  (:B --> :S))

(define-grammar *grammar-8* :S
  (:S --> :E)
  (:E --> :T :E1)
  (:E1 --> #\+ :E)
  (:E1 --> :eps)
  (:T --> :F :T1)
  (:T1 --> #\* :T)
  (:T1 --> :eps)
  (:F  --> #\( :E #\))
  (:F  --> :id)
  (:F --> :num))


;;; First set tests for grammars 1-8

(define-case-test (first-set-test () :all-test-name first-set-tests)
  (*grammar-1*
   (check-first-sets *grammar-1*
     ('A (list :c :d))
     ('B (list :c :eps))
     ('C (list :c))))
  (*grammar-2*
   (check-first-sets *grammar-2*
     (:expr    (list :id #\())
     (:sum     (list :id #\())
     (:product (list :id #\())
     (:value   (list :id #\())))
  (*grammar-3*
   (check-first-sets *grammar-3*
    (:a (list :eps))))
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
      (funcall (eval (second f)) -1))))
  (*grammar-6*
   (check-first-sets *grammar-6*
     (:E (list #\( :id))
     (:T (list #\( :id))
     (:F (list #\( :id))
     (:X (list #\+ :eps))
     (:Y (list #\* :eps))))
  (*grammar-7*
   (check
     (not (gethash :S (first-sets *grammar-7*)))
     (not (gethash :A (first-sets *grammar-7*)))
     (not (gethash :B (first-sets *grammar-7*)))))
  (*grammar-8*
   (check-first-sets *grammar-8*
     (:S (list #\( :id :num))
     (:E (list #\( :id :num))
     (:T (list #\( :id :num))
     (:F (list #\( :id :num))
     (:T1 (list #\* :eps))
     (:E1 (list #\+ :eps)))))
     

;;; Follow set tests for grammars 1-8

(define-case-test (follow-set-test () :all-test-name follow-set-tests)
    (*grammar-1*
     (check-follow-sets *grammar-1*
       ('A (list :$))
       ('B (list :c))
       ('C (list :c :$))))
    (*grammar-2*
     (check-follow-sets *grammar-2*
       (:expr (list #\) :$))
       (:sum (list #\) :$))
       (:product (list #\- #\+ #\) :$))
       (:value (list #\- #\+ #\) :$))))
    (*grammar-3*
     (check-follow-sets *grammar-3*
       (:a (list :$))))
    (*grammar-6*
     (check-follow-sets *grammar-6*
       (:E (list #\) :$))
       (:X (list #\) :$))
       (:Y (list #\+ #\) :$))
       (:T (list #\+ :$ #\)))
       (:F (list #\* #\+ #\) :$))))
    (*grammar-7*
     (check-follow-sets *grammar-7*
       (:S (list :$))
       (:A (list :$))
       (:B (list :$))))
    (*grammar-8*
     (check-follow-sets *grammar-8*
       (:S (list :$))
       (:E (list #\) :$))
       (:E1 (list #\) :$))
       (:T (list #\+ #\) :$))
       (:T1 (list #\+ #\) :$))
       (:F (list #\* #\+ #\) :$)))))


;;; Grammar 8
;;; An LL(1) Grammar
(define-ll-1-parser grammar-8-parser :S
  (:S --> :E)
  (:E --> :T :E1)
  (:E1 --> #\+ :E)
  (:E1 --> :eps)
  (:T --> :F :T1)
  (:T1 --> #\* :T)
  (:T1 --> :eps)
  (:F  --> #\( :E #\))
  (:F  --> :id)
  (:F --> :num))

(define-accept-test grammar-8-accept-test grammar-8-parser ()
  (list :id :$)
  (list :num :$)
  (list :num #\+ :num :$)
  (list :id #\+ :id :$)
  (list #\( :id #\) #\+ #\( #\( :id #\) #\) :$)
  (list #\( :id #\* :id #\) :$))

(define-reject-test grammar-8-reject-test grammar-8-parser ()
  (list :$)
  (list #\+ :num)
  (list #\* :num)
  (list #\( :num :$)
  (list #\) :$)
  (list #\( #\( #\( :id #\+ :id #\) #\+ :num #\) :$))

;;; Grammar 9:
;;; Another LL(1) Grammar
(define-ll-1-parser grammar-9-parser :S
  (:S --> :S1)
  (:S1 --> :F)
  (:S1 --> #\( :S1 #\+ :F #\))
  (:F --> :a))

(define-accept-test grammar-9-accept-test grammar-9-parser ()
  (list :a :$)
  (list #\( :a #\+ :a #\) :$)
  (list #\( #\( :a #\+ :a #\) #\+ :a #\) :$)
  (list #\( #\(  #\( :a #\+ :a #\) #\+ :a #\) #\+ :a #\) :$))

(define-reject-test grammar-9-reject-test grammar-9-parser ()
  (list :$)
  (list #\( :a #\) :$)
  (list #\) :a #\) :$)
  (list #\( #\( :a #\+ :a #\) #\+ #\( :a #\+ :a #\) #\) :$))

;;; Grammar 10:
;;; Testing predefined functions as tokens
(define-ll-1-parser grammar-10-parser :S
  (:S --> :S1)
  (:S1 --> :F)
  (:S1 --> #\( :S1 #\+ :F #\))
  (:F --> #'integerp))

(define-accept-test grammar-10-accept-test grammar-10-parser ()
  (list 1 :$)
  (list #\( 2 #\+ 3 #\) :$)
  (list #\( #\( 4 #\+ 5 #\) #\+ 6 #\) :$)
  (list #\( #\(  #\( 7 #\+ 8 #\) #\+ 9 #\) #\+ 10 #\) :$))

(define-reject-test grammar-10-reject-test grammar-10-parser ()
  (list :$)
  (list #\( #\) :$)
  (list #\) 1 #\) :$)
  (list #\( #\( 2 #\+ 3 #\) #\+ #\( 4 #\+ 5 #\) #\) :$)
  (list "1" :$)
  (list #\( :2 #\+ :a #\) :$)
  (list #\( #\( 3 #\+ 4 #\) #\+ :a #\) :$)
  (list #\( #\(  #\( 5 #\+ :a #\) #\+ 6 #\) #\+ 7 #\) :$))

;;; Test combinations

(define-test accept-tests ()
  (combine-results
    (grammar-8-accept-test)
    (grammar-9-accept-test)
    (grammar-10-accept-test)))

(define-test reject-tests ()
  (combine-results
    (grammar-8-reject-test)
    (grammar-9-reject-test)
    (grammar-10-reject-test)))

(define-test parser-test ()
  (combine-results
    (def-grammar-test)
    (first-set-tests)
    (follow-set-tests)
    (accept-tests)
    (reject-tests)))
