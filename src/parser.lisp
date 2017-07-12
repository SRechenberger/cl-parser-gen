(in-package :de.srechenberger.cl-parser-gen.parser)

;;;; Parser generator utilities
;;; Auxiliary SPECIAL variables
(defparameter *first-sets* nil)
(defparameter *follow-sets* nil)
(defparameter *non-terminals* nil)
(defparameter *grammar* nil)

;;; The only way to use the latter variables!
(defmacro with-grammar (grammar &body body)
  "Initiates the envrironment with the given GRAMMAR rules;
if any of the variables *FIRST-SETS*, *FOLLOW-SETS*, *NON-TERMINALS*, or *GRAMMAR* is set,
an error is throws (via ASSERT)"
  `(progn (assert (not (or *first-sets*
		   *follow-sets*
		   *non-terminals*
		   *grammar*)))
	  (let ((*grammar* ,grammar)
		(*non-terminals* (mapcar #'first ,grammar))
		(*first-sets* (make-hash-table))
		(*follow-sets* (make-hash-table)))
	    ,@body)))
;;; The grammar class,
;;; consisting of a START-SYMBOL,
;;; the set of NON-TERMINALS,
;;; the set of production RULES,
;;; and the FIRST- and FOLLOW-SETS.
(defclass grammar ()
  ((start-symbol
    :initarg :start-symbol
    :initform (error "Must supply a start symbol.")
    :reader start-symbol
    :documentation "The start symbol of the grammar.")
   (non-terminals
    :initarg :non-terminals
    :initform (error "Must supply the set of non terminals.")
    :reader non-terminals
    :documentation "The set of NON-TERMINALS; all other symbols are terminals.")
   (rules
    :initarg :rules
    :initform (error "Must supply at least one rule.")
    :reader rules
    :documentation "The production rules of the grammar.")
   (first-sets
    :initarg :first-sets
    :initform (error "Must supply the first sets.")
    :reader first-sets
    :documentation "The FIRST lookahead set for every non-terminal")
   (follow-sets
    :initarg :follow-sets
    :initform (error "Must supply the follow sets.")
    :reader follow-sets
    :documentation "The FOLLOW lookahead set for every non-terminal")))


(defun make-rules (rules)
  "Takes a list of rules written as
 (NON-TERMINAL --> SYMBOL*)
and converts them to a list
 (NON-TERMINAL (SYMBOL*))"
  (loop
     for rule in rules
     do (when (not (equal (second rule) '-->))
	  (error (format nil "Syntax error in ~a." rule)))
     collect (list (car rule) (cddr rule))))

(defmacro define-grammar (name s-symbol &body rules)
  "Defines a variable with name NAME; keep naming conventions in mind.
It needs the start symbol S-SYMBOL and a body of RULES of the form
 (NON-TERMINAL --> SYMBOL*)
where a SYMBOL may be any terminal or non-terminal" 
  (let* ((correct-rules (make-rules rules))
	 (nts (remove-duplicates (mapcar #'first correct-rules)))
	 (first-follow (with-grammar correct-rules
			 (dolist (nt nts)
			   (first-set nt))
			 (dolist (nt nts)
			   (follow-set nt))
			 (list *first-sets* *follow-sets*))))
    `(defparameter ,name
       (make-instance
	'grammar
	:start-symbol ',s-symbol
	:non-terminals ',nts
	:rules ',correct-rules
	:first-sets ,(first first-follow)
	:follow-sets ,(second first-follow))
       ,(format nil "GRAMMAR ~a~%START-SYMBOL: ~s~%NON-TERMINALS: {~{~s~^, ~}}~%RULES:~%~{ ~{~s~^ ~}~^~%~}~%"
		name
		s-symbol
		nts
		rules))))
		    

;; DUMMY
(defun follow-set (symb)
  nil)

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
  "Calculates the FIRST set of the current grammar stored in *GRAMMAR*."
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

