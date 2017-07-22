(in-package :de.srechenberger.cl-parser-gen.tokenizer)

(define-condition tokenizer-error (error)
  ((text :initarg :text
	 :reader text)))

(defun first-match (regex input)
  "Finds the first match for REGEX in INPUT and returns the rest of INPUT,
after the match and all captures."
  ;; (format t "(first-match ~s ~s)~%" regex input) 
  (multiple-value-bind (begin end match-begins match-ends) (cl-ppcre:scan regex input)
    (when (and begin (= 0 begin)) ;; when there is any match and it begins at index 0
      (list (subseq input end)
	    (loop
	       for match-begin across match-begins
	       for match-end across match-ends
	       collect (subseq input match-begin match-end))))))

(defmacro define-tokenizer (name params &rest rules)
  "Given a NAME and a set of RULES, this macro expands to a tokenizer function of name NAME,
which takes a string INPUT and tries to tokenize it according to the given rules.
The rules consist of a regular expression and a function, which will be applied to the result
vector of the regex; hence the ID function, if you want to get the plain match."
  ;; (format t "~s~%" rules)
  (with-gensyms (input tmp)
    `(defun ,name (,input ,@params)
       (let ((,tmp))
	 (loop
	    while (< 0 (length ,input))
	    collect
	      (or
	       ,@(loop
		    for rule in rules
		    collect (destructuring-bind (regex func) rule
			      `(progn
				 (setf ,tmp (first-match ,regex ,input))
				 (when ,tmp
				   (setf ,input (first ,tmp))
				   (apply ,func (second ,tmp))))))
	       (error 'tokenizer-error
		      :text (format nil "~a"
					(subseq ,input 0 (min (length ,input) 20))))))))))
     
(defun id (x &rest xs)
  "The Identity function. May get handy sometime."
  (cond
    (xs (cons x xs))
    (t  x)))
