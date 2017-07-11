(in-package :de.srechenberger.parser)

(defparameter *test-grammar*
  (list (list 'A (list 'B 'C))
	(list 'A (list :d :e))
	(list 'B (list 'C))
	(list 'C (list :c))
	(list 'B (list :eps))))

(defparameter *first-sets* nil)
(defparameter *non-terminals* nil)
(defparameter *grammar* nil)

(defmacro unions (&body sets)
  `(reduce #'union (list ,@sets)))

(defun non-term-p (symb)
  (member symb *non-terminals*))

(defun term-p (symb)
  (and (not (listp symb))
       (not (non-term-p symb))))

(defparameter *depth* 0)
(defparameter *debug* nil)
(defun first-set (symb)
  (let ((*depth* (1+ *depth*)))
    (cond
      ;; I: SYMB is a terminal
      ((term-p symb)
       (when *debug* (format t "I <~a> ~s ~%" *depth* symb))
       (list symb))
      ;; II: SYMB is a single, non-terminal symbol
      ((non-term-p symb) 
       (when *debug* (format t "II <~a> ~s ~%" *depth* symb))
       (let ((found (gethash symb *first-sets*)))
	 (if found found
	     (let ((calculated
		    (reduce #'union
			    (loop
			       for (head body) in *grammar*
			       ;; do (format t "<~a> (HEAD BODY) == (~s ~s)~%" *depth* head body)
			       when (equal head symb)
			       collect (first-set body)))))
	       (setf (gethash symb *first-sets*) calculated)))))
      ;; III: SYMB is NIL (an empty list)
      ((not symb)
       (when *debug* (format t "III <~a> ~s~%" *depth* symb))
       (list :eps))
      ;; IV: SYMB is a list of symbols
      ((listp symb)
       (when *debug* (format t "IV <~a> ~s~%" *depth* symb))
       (loop
	  for h in symb
	  ;; do (format t "H == ~s~%" h)
	  for f = (first-set h)
	  ;; do (format t "<~a> (FIRST-SET ~s) == ~s~%" *depth* h f)
	  collect f into bucket
	  ;; do (format t "<~a> BUCKET == ~s~%" *depth* bucket)
	  while (member :eps f)
	  finally (return (if (member :eps f)
			      (reduce #'union bucket)
			      (remove :eps (reduce #'union bucket)))))))))

(defmacro with-grammar (grammar &body body)
  `(let ((*grammar* ,grammar)
	 (*non-terminals* (mapcar #'first ,grammar))
	 (*first-sets* (make-hash-table)))
     ,@body))
		  
		
	     
  
			    
			  
	       
		    
  
