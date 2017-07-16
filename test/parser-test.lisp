(in-package :de.srechenberger.cl-parser-gen.parser-test)

(defun set-equal (set-1 set-2)
  (and (subsetp set-1 set-2)
       (subsetp set-2 set-1)))

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


(defun has-first-set (symb set grammar)
  `(set-equal (gethash ',symb (first-sets ,grammar)) ,set))

(defmacro check-first-sets (grammar &body body)
  `(check
     ,@(loop
	  for (symb set) in body
	  collect `(set-equal (gethash ,symb (first-sets ,grammar)) ,set))))

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
      (funcall (eval (second f)) -1))))
  (*grammar-6*
   (check
     (set-equal (gethash :E (first-sets *grammar-6*)) (list #\( :id))
     (set-equal (gethash :T (first-sets *grammar-6*)) (list #\( :id))
     (set-equal (gethash :F (first-sets *grammar-6*)) (list #\( :id))
     (set-equal (gethash :X (first-sets *grammar-6*)) (list #\+ :eps))
     (set-equal (gethash :Y (first-sets *grammar-6*)) (list #\* :eps))))
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
     

(defun has-follow-set (symb set grammar)
  `(set-equal (gethash ',symb (follow-sets ,grammar)) ,set))

(defmacro check-follow-sets (grammar &body body)
  `(check
     ,@(loop
	  for (symb set) in body
	  collect `(set-equal (gethash ,symb (follow-sets ,grammar)) ,set))))
     

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

(define-ll-1-parser grammar-8-parser *grammar-8*)

(defmacro define-accept-test (name parser parameters &body inputs)
  `(define-test ,name ,parameters
    (check
      ,@(mapcar #'(lambda (input) `(,parser ,input)) inputs))))

(defmacro define-reject-test (name parser parameters &body inputs)
  `(define-test ,name ,parameters
     (check
       ,@(mapcar #'(lambda (input) `(not (,parser ,input))) inputs))))

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
  
(define-test accept-tests ()
  (combine-results
    (grammar-8-accept-test)))

(define-test reject-tests ()
  (combine-results
    (grammar-8-reject-test)))

(define-test parser-test ()
  (combine-results
    (def-grammar-test)
    (first-set-tests)
    (follow-set-tests)
    (accept-tests)
    (reject-tests)))
