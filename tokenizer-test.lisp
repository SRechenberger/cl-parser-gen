(in-package :de.srechenberger.tokenizer-test)

;; Lex some whitespaces

(define-tokenizer empty-input ()
  ("(.*)" #'id))

(define-tokenizer whitespaces ()
    ("(\\s*)"
     #'(lambda (res) (length (first res)))))

(define-tokenizer pairs ()
    ("\\(([^,]+),([^,]+)\\)" #'id))

(define-tokenizer integers ()
    ("(\\d+)\\s*"
     #'(lambda (res)
	 (parse-integer (first res)))))

(define-tokenizer choosing ()
  ("(a|A)"
   #'(lambda (x) :A))
  ("(b|B)"
   #'(lambda (x) :B)))

(define-tokenizer calcs ()
  ("add\\s*\\((\\d+),(\\d+)\\)"
   #'(lambda (res)
       (apply #'+ (mapcar #'parse-integer res))))
  ("sub\\s*\\((\\d+),(\\d+)\\)"
   #'(lambda (res)
       (apply #'- (mapcar #'parse-integer res)))))

(define-tokenizer params-1 (a b)
  ("(\\d+)"
   #'(lambda (res)
       (+ a (* b (parse-integer (first res)))))))

(define-tokenizer params-2 (f)
  ("([^;]*);"
   #'(lambda (res)
       (funcall f (first res)))))

(deftest whitespaces-test ()
  (check
    (equal (whitespaces "") nil)
    (equal (whitespaces " ") (list 1))
    (equal (whitespaces "  ") (list 2))))

(deftest pairs-test ()
  (check
    (equal (pairs "(1,2)") (list (list "1" "2")))
    (equal (pairs "(10,20)") (list (list "10" "20")))
    (equal (pairs "(A,20)") (list (list "A" "20")))))

(deftest integer-simple-test ()
  (check
    (equal (integers "1 2 3 4 5") (list 1 2 3 4 5))
    (equal (integers "123 456 789") (list 123 456 789))))

(deftest choosing-test ()
  (check
    (equal (choosing "aA") (list :A :A))
    (equal (choosing "ba") (list :B :A))))

(deftest calc-test ()
  (check
    (equal (calcs "add(4,3)") (list 7))
    (equal (calcs "sub(4,3)") (list 1))))

(deftest params-test ()
  (check
    (equal (params-1 "10" 2 1) (list 12))
    (equal (params-2 ";1;11;111;1111;" #'length) (list 0 1 2 3 4))))

(deftest run-test ()
  (combine-results
    (whitespaces-test)
    (pairs-test)
    (integer-simple-test)
    (choosing-test)
    (calc-test)
    (params-test)))

