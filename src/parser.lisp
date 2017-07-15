(in-package :de.srechenberger.cl-parser-gen.parser)

;;;; Parser generator utilities
;;; Auxiliary SPECIAL variables
(defparameter *first-sets* nil)
(defparameter *follow-sets* nil)
(defparameter *non-terminals* nil)
(defparameter *grammar* nil)
(defparameter *depth* 0)
(defparameter *debug* nil)
(defparameter *start-symbol* nil)

(defmacro unions (&body sets)
  `(reduce #'union (list ,@sets)))

(defun non-term-p (symb)
  (member symb *non-terminals*))

(defun term-p (symb)
  (and (not (listp symb))
       (not (non-term-p symb))))
  ;;     (not (equal symb :eps))))

(defun predicate-p (symb)
  (and (listp symb)
       (or
	(equal (car symb) 'function)
	(equal (car symb) 'lambda))))
       


;;; The only way to use the latter variables!
(defmacro with-grammar (grammar  &body body)
  "Initiates the envrironment with the given GRAMMAR rules;
if any of the variables *FIRST-SETS*, *FOLLOW-SETS*, *NON-TERMINALS*, or *GRAMMAR* is set,
an error is throws (via ASSERT)"
  `(progn (assert (not (or *first-sets*
			   *follow-sets*
			   *non-terminals*
			   *grammar*)))
	  (let ((*grammar* ,grammar)
		(*non-terminals* (mapcar #'first ,grammar))
		(*start-symbol* nil)
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
where a SYMBOL may be any terminal or non-terminal and also PREDICATES.
If the parser is generated, TERMINALS will be compared with a then given EQUAL predicate,
if the terminal is a PREDICATE, it will be applied to the seen token, and accepted on a non nil result."
  (let* ((correct-rules (make-rules rules))
	 (nts (remove-duplicates (mapcar #'first correct-rules)))
	 (first-follow (with-grammar correct-rules
			 (dolist (nt nts)
			   (first-set nt))
			 (setf *start-symbol* s-symbol)
			 (when *debug* (format t "*START-SYMBOL* ==  ~S ~%" *start-symbol*))
			 (pushnew :$ (gethash s-symbol *follow-sets*))
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
       ,(format
	 nil
	 "GRAMMAR ~a~%START-SYMBOL: ~s~%NON-TERMINALS: {~{~s~^, ~}}~%RULES:~%~{ ~{~s~^ ~}~^~%~}~%"
	 name
	 s-symbol
	 nts
	 rules))))
		    



(defparameter *first-stack* nil)

(defun first-set (symb)
  "Calculates the FIRST set of the current grammar stored in *GRAMMAR*."
  (unless (member symb *first-stack*)
    (when *debug* (format t "~s~%" *first-stack*))
    (let ((*depth* (1+ *depth*))
	  (*first-stack* (cons symb *first-stack*)))
      (cond
	;; I: SYMB is a predicate
	((predicate-p symb)
	 (when *debug* (format t "I' <~a> ~s ~%" *depth symb))
	 (list symb))
	;; II: SYMB is a terminal
	((term-p symb)
	 (when *debug* (format t "II <~a> ~s ~%" *depth* symb))
	 (list symb))
	;; III: SYMB is a single, non-terminal symbol
	((non-term-p symb) 
	 (when *debug* (format t "III <~a> ~s ~%" *depth* symb))
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
	;; IV: SYMB is NIL (an empty list)
	((not symb)
	 (when *debug* (format t "IV <~a> ~s~%" *depth* symb))
	 (list :eps))
	;; V: SYMB is a list of symbols
	((listp symb)
	 (when *debug* (format t "V <~a> ~s~%" *depth* symb))
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
				(remove :eps (reduce #'union bucket))))))))))


(defparameter *follow-stack* nil)

(defun follow-set (symb)
  (when (not (non-term-p symb))
    (error (format nil "~s is no non-terminal." symb)))
  (when *debug* (format t "~S~%" *follow-stack*))
  (unless (member symb *follow-stack*)
    (let ((*follow-stack* (cons symb *follow-stack*)))
      (loop
	 for (head body) in (remove-if-not #'(lambda (r) (member symb (second r))) *grammar*)
	 do (loop
	       for s on body
	       do (progn
		    (when (and (non-term-p (first s))
			       (second s))
		      (dolist (f (remove :eps (first-set (second s))))
			(pushnew f (gethash (first s) *follow-sets*))))
		    (when (and (non-term-p (first s))
			       (or (not (second s))
				   (member :eps (first-set (second s)))))
		      (dolist (f (follow-set head))
			(pushnew f (gethash (first s) *follow-sets*)))))))))
  (gethash symb *follow-sets*))

(defmacro forall-in (sym sequence &body body)
  `(every #'(lambda (,sym) ,@body) ,sequence)) 

(defmacro exists-in (sym sequence &body body)
  `(some #'(lambda (,sym) ,@body) ,sequence))

(defvar *eps-p-stack* nil)
;; eps(alpha) =
;; FORALL a in alpha .
;;   a is a non terminal AND EXISTS (a --> beta) in P . eps(beta)
(defun eps-p (alpha grammar)
  (when *debug* (format t "STACK ~s~%" *eps-p-stack*))
  (or (equal alpha (list :eps))
      (forall-in a alpha
	(and (member a (non-terminals grammar))
	     (exists-in rule (rules grammar)
	       (when *debug* (format t "RULE ~s~%" rule))
	       (and (equal (first rule) a)
		    (not (member a *eps-p-stack*))
		    (let ((*eps-p-stack* (cons a *eps-p-stack*)))
		      (eps-p (second rule) grammar))))))))

    

(defun make-parser-table (grammar)
  (let ((parser-table (make-hash-table :test #'equal)))
    (with-grammar (rules grammar)
      (dolist (rule (rules grammar))
	(dolist (symb (first-set (second rule)))
	  (when (gethash (list (first rule) symb) parser-table)
	    (error
	     (format
	      nil
	      "I Entry (~s,~s) already [~s] (new: ~s). Your grammar is not LL(1)!"
	      (first rule)
	      symb
	      (gethash (list (first rule) symb) parser-table)
	      (second rule))))
	  (setf (gethash (list (first rule) symb) parser-table) (second rule)))
	(when (eps-p (second rule) grammar)
	  (dolist (symb (follow-set (first rule)))	  
	    (when (gethash (cons (first rule) symb) parser-table)
	      (error
	       (format
		nil
		"II Entry (~s,~s) already [~s] (new: ~s). Your grammar is not LL(1)!"
		(first rule)
		symb
		(gethash (list (first rule) symb) parser-table)
		(second rule))))
	    (setf (gethash (list (first rule) symb) parser-table) (second rule)))))
      parser-table)))

(defmacro define-ll-1-parser (grammar)
  "Given some rules like
(A --> #'evenp :c)
(A --> :a      :b)
the parser code may look like this,
where TOPP checks, if the given item is atop the STACK
and STACK is the item stack of the parser.
(cond
  ...
  ((topp A)
   (cond
     ((funcall (eval #'evenp) look-ahead)
      (push :c stack)
      (push #'evenp stack))
     ((equal :a look-ahead)
      (push :b stack)
      (push :a stack))))
  ...)"
  (error "DEFINE-PARSER NOT IMPLEMENTED YET"))


