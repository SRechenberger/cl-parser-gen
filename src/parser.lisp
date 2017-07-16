(in-package :de.srechenberger.cl-parser-gen.parser)

;;;; Parser generator utilities
;;; Auxiliary SPECIAL variables
(defparameter *depth* 0)
(defparameter *debug* nil)
(defparameter *start-symbol* nil)

(defmacro unions (&body sets)
  `(reduce #'union (list ,@sets)))

(defun non-term-p (symb grammar)
  (member symb (non-terminals grammar)))

(defun term-p (symb grammar)
  (and (not (listp symb))
       (not (non-term-p symb grammar))))

(defun predicate-p (symb)
  (and (listp symb)
       (or
	(equal (car symb) 'function)
	(equal (car symb) 'lambda))))

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
	 (nts (remove-duplicates (mapcar #'first correct-rules))))
    `(defparameter ,name
       (let ((g (make-instance
		 'grammar
		 :start-symbol ',s-symbol
		 :non-terminals ',nts
		 :rules ',correct-rules
		 :first-sets (make-hash-table)
		 :follow-sets (make-hash-table))))
	 (dolist (nt ',nts)
	   (first-set nt g))
	 (pushnew :$ (gethash (start-symbol g) (follow-sets g)))
	 (dolist (nt ',nts)
	   (follow-set nt g))
	 g)
       ,(format
	 nil
	 "GRAMMAR ~a~%START-SYMBOL: ~s~%NON-TERMINALS: {~{~s~^, ~}}~%RULES:~%~{ ~{~s~^ ~}~^~%~}~%"
	 name
	 s-symbol
	 nts
	 rules))))
  

	 


(defparameter *first-stack* nil)

(defun first-set (symb grammar)
  "Calculates the FIRST set of the current grammar stored in *GRAMMAR*."
  (unless (member symb *first-stack*)
    (when *debug* (format t "~s~%" *first-stack*))
    (let ((*depth* (1+ *depth*))
	  (*first-stack* (cons symb *first-stack*)))
      (cond
	;; I: SYMB is a predicate
	((predicate-p symb)
	 (when *debug* (format t "I' <~a> ~s ~%" *depth* symb))
	 (list symb))
	;; II: SYMB is a terminal
	((term-p symb grammar)
	 (when *debug* (format t "II <~a> ~s ~%" *depth* symb))
	 (list symb))
	;; III: SYMB is a single, non-terminal symbol
	((non-term-p symb grammar) 
	 (when *debug* (format t "III <~a> ~s ~%" *depth* symb))
	 (let ((found (gethash symb (first-sets grammar))))
	   (if found found
	       (let ((calculated
		      (reduce #'union
			      (loop
				 for (head body) in (rules grammar)
				 ;; do (format t "<~a> (HEAD BODY) == (~s ~s)~%" *depth* head body)
				 when (equal head symb)
				 collect (first-set body grammar)))))
		 (setf (gethash symb (first-sets grammar)) calculated)))))
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
	    for f = (first-set h grammar)
	    ;; do (format t "<~a> (FIRST-SET ~s) == ~s~%" *depth* h f)
	    collect f into bucket
	    ;; do (format t "<~a> BUCKET == ~s~%" *depth* bucket)
	    while (member :eps f)
	    finally (return (if (member :eps f)
				(reduce #'union bucket)
				(remove :eps (reduce #'union bucket))))))))))


(defparameter *follow-stack* nil)
(defun follow-set (symb grammar)
  (when (not (non-term-p symb grammar))
    (error (format nil "~s is no non-terminal." symb)))
  (when *debug* (format t "~S~%" *follow-stack*))
  (unless (member symb *follow-stack*)
    (let ((*follow-stack* (cons symb *follow-stack*)))
      (loop
	 for (head body) in (remove-if-not #'(lambda (r) (member symb (second r))) (rules grammar))
	 do (loop
	       for s on body
	       do (progn
		    (when (and (non-term-p (first s) grammar)
			       (second s))
		      (dolist (f (remove :eps (first-set (second s) grammar)))
			(pushnew f (gethash (first s) (follow-sets grammar)))))
		    (when (and (non-term-p (first s) grammar)
			       (or (not (second s))
				   (member :eps (first-set (second s) grammar))))
		      (dolist (f (follow-set head grammar))
			(pushnew f (gethash (first s) (follow-sets grammar))))))))))
  (gethash symb (follow-sets grammar)))

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
    (dolist (rule (rules grammar))
      (dolist (symb (remove :eps (first-set (second rule) grammar)))
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
	(dolist (symb (follow-set (first rule) grammar))
	  ;;(format t "FOLLOW(~s) == ~s~%" (first rule) (follow-set (first rule) grammar))
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
    parser-table))

#|
  Given some rules like
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
  ...)
|#


(defun reduce-result-stack (result-stack)
  (when *debug* (format t "(REDUCE-RESULT-STACK ~s)~%(CAAR RESULT-STACK) == ~s~%"
			result-stack (caar result-stack)))
  (if (and (= 0 (caar result-stack))
	   (< 1 (length result-stack)))
      (let ((tmp (cdar result-stack)))
	(when *debug* (format t "TMP == ~s~%" tmp))
	(pop result-stack)
	(when tmp
	  (setf (cdar result-stack) (append (cdar result-stack) (list tmp))))
	(decf (caar result-stack))
	(reduce-result-stack result-stack))
      result-stack))

(defun debulk (list)
  (if (listp list)
      (if (= 1 (length list))
	  (debulk (car list))
	  (mapcar #'debulk list))
      list))

(defmacro define-ll-1-parser (name s-symbol &body rules)
    (with-gensyms (work-stack result-stack idx len top input reject)
      (let* ((correct-rules (make-rules rules))
	     (nts (remove-duplicates (mapcar #'first correct-rules)))
	     (grm (let ((g (make-instance
			    'grammar
			    :start-symbol s-symbol
			    :non-terminals nts
			    :rules correct-rules
			    :first-sets (make-hash-table)
			    :follow-sets (make-hash-table))))
		    (dolist (nt nts)
		      (first-set nt g))
		    (pushnew :$ (gethash (start-symbol g) (follow-sets g)))
		    (dolist (nt nts)
		      (follow-set nt g))
		    g))
	     (p-table (make-parser-table grm)))
	(when *debug* (maphash #'(lambda (k v) (format t "[~s] ==> ~s~%" k v)) p-table))
	`(defun ,name (,input &key
				(log-stream *standard-output* log-p)
				stepwise
				reject-by-error
				debulk-result)
	   (do* ((,work-stack (list ,(start-symbol grm) :$))
		 (,result-stack (list (cons 2 nil)))
		 ;; The current input position
		 (,idx 0)
		 (,len (length ,input))
		 (,top (car ,work-stack) (car ,work-stack))
		 (,reject nil))
		;; If INPUT is empty
		((or (>= ,idx ,len)
		     ,reject)
		 ;; and the WORK-STACK is empty
		 (cond
		   (,reject nil)
		   ((not ,work-stack)
		    ;; return the result
		    (if debulk-result
			 (debulk (cdar ,result-stack))
			 (cdar ,result-stack)))
		   (t (when reject-by-error (error "FUCK")))))
	     
	     (when (or stepwise log-p)
	       (format log-stream "TOP: ~s~%STACK: ~s~%INPUT: ~s~%RESULT: ~s~%"
		       ,top ,work-stack (subseq ,input ,idx) ,result-stack))
	     
	     (when stepwise
	       (unless (y-or-n-p "Continue?") (return nil)))
	     
	     (setf ,result-stack (reduce-result-stack ,result-stack))
	     (cond
	       ;; Try to consume input
	       ((not (member ,top (list ,@(non-terminals grm))))
		(if (or (equal (elt ,input ,idx) ,top)
			(and (functionp ,top)
			     (funcall ,top (elt ,input ,idx)))
			(equal :eps ,top))
		    (progn
		      (unless (equal :eps ,top)
			(setf (cdar ,result-stack)
			      (append (cdar ,result-stack)
				      (list (elt, input ,idx))))
			(incf ,idx))
		      (pop ,work-stack)
		      (decf (caar ,result-stack)))
		    (if reject-by-error
			 (error (format nil "Expected ~s, but found ~s at position ~d."
					,top
					(elt ,input ,idx)
					,idx))
			 (return nil))))
	       ;; Expansion
	       ,@(loop
		    for k being the hash-keys in p-table
		    for v being the hash-values in p-table
		    collect
		      (if (predicate-p (second k))
			  `((and (equal ,(first k) ,top)
				 (funcall ,(second k) (elt ,input ,idx)))
			    (pop ,work-stack)
			    (setf ,work-stack (append (list ,@v) ,work-stack))
			    (push (cons ,(length v) nil) ,result-stack))
			  `((and (equal ,(first k) ,top)
				 (equal ,(second k) (elt ,input, idx)))
			    (pop ,work-stack)
			    (setf ,work-stack (append (list ,@v) ,work-stack))
			    (push (cons ,(length v) nil) ,result-stack))))
	       ;; Error
	       (t (if reject-by-error
		       (error (format nil "No entry in the parser table for (~s,~s)."
				      ,top
				      (elt ,input ,idx)))
		       (return nil)))))))))
       
     


