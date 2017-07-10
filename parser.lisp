(in-package :de.srechenberger.parser)

(defparameter *test-grammar*
  (list (list 'A (list 'B 'C))
	(list 'A (list :d :e))
	(list 'B (list 'C))
	(list 'C (list :c))
	(list 'B (list))))

(defparameter *first-sets* (make-hash-table))
(defparameter *non-terminals* nil)
(defparameter *grammar* nil)

(defmacro unions (&body sets)
  `(reduce #'union (list ,@sets)))

(defun non-term-p (symb)
  (member symb *non-terminals*))

(defun term-p (symb)
  (and (not (listp symb))
       (not (non-term-p symb))))

(defun first-set (symb)
  (cond
    ((term-p symb)
     (format t "I ~s~%" symb)
     (list symb))
    ((non-term-p symb) 
     (format t "II ~s~%" symb)
     (let ((found (gethash symb *first-sets*)))
       (if found found
	   (let ((calculated
		  (reduce #'union
			  (loop
			     for (head body) in *grammar*
			     when (equal head symb)
			     collect (first-set body)))))
	     (setf (gethash symb *first-sets*) calculated)))))
    ((and (listp symb) (not symb))
     (format t "III ~s~%" symb)
     (list :eps))
    ((listp symb)
     (format t "IV ~s~%" symb)
     (loop
	for h in symb
	with f = (first-set h)
	collect f into bucket
	while (not (member :eps f))
	finally (return (if (member :eps f)
			    (reduce #'union bucket)
			    (remove :eps (reduce #'union bucket))))))))

(defmacro with-grammar (grammar &body body)
  `(let ((*grammar* ,grammar)
	 (*non-terminals* (mapcar #'first ,grammar)))
     ,@body))
		  
		
	     
  
			    
			  
	       
		    
  
