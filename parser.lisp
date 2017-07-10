(in-package :de.srechenberger.parser)

(defparameter *test-grammar*
  '((A (B C))
    (B (C))
    (C :c)))

(defvar *first-sets* nil)
(defvar *non-terminals* nil)
(defvar *grammar* nil)

(defun first-set (alpha visited)
  (cond
    ((not (find alpha *non-terminals*))
     alpha)
    ((not (find alpha visited))
     (push alpha visited)
     (nconc (mapcar #'(lambda (bodysym)
			(first-set bodysym visited))
		    (mapcar #'cdr
			    (remove-if-not #'(lambda (rule)
					       (equal (car rule) alpha))
					   *grammar*))))
    (t (list))))
