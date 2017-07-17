[![Build Status](https://travis-ci.org/SRechenberger/cl-parser-gen.svg?branch=master)](https://travis-ci.org/SRechenberger/cl-parser-gen)

# cl-parser-gen

## de.srechenberger.cl-parser-gen.tokenizer
Generate a tokenizer function by giving a list of regular expressions (using [cl-ppcre](http://weitz.de/cl-ppcre/))
and a function, which will be applied to captured objects.
The regular expression will be applied to the **beginning** of the input string in the listed order.

E.g.:

    (define-tokenizer pairs ()
      ("\\(([^,]+),([^,]+)\\)" #'id))
      
This will generate the function `pairs`, which takes, save for the input, no argument,
and returns, applied to e.g. `"(1,2)"`, since `#'id` is given as the function to be applied to the results, `(list (list "1" "2"))`.

    (define-tokenizer calcs ()
      ("add\\s*\\((\\d+),(\\d+)\\)"
       #'(lambda (res)
            (apply #'+ (mapcar #'parse-integer res))))
      ("sub\\s*\\((\\d+),(\\d+)\\)"
       #'(lambda (res)
            (apply #'- (mapcar #'parse-integer res)))))
          
The generated function `calc` takes a string like `"add(1,2)"` or `"sub(1,2)"` and returns `(list 3)` or `(list -1)` respectively.

    (define-tokenizer integers ()
      ("(\\d+)\\s*"
       #'(lambda (res)
            (parse-integer (first res)))))
            
Generates the function `integers`, which takes a string like `"1 2 3 4 5"` or `"123 456 789"` and returns `(list 1 2 3 4 5)` or `(list 123 456 789)` respectively.

You may also add custom arguments to the function:

    (define-tokenizer params-1 (a b)
      ("(\\d+)"
       #'(lambda (res)
            (+ a (* b (parse-integer (first res)))))))
            
`params-1` takes two additional arguments *after* the input, thus `(params-1 "10" 2 1)` returns `(list 12)`, for `(+ 2 (* 1 10))` equals `12`.

## de.srechenberger.cl-parser-gen.parser
Generate a Parser by giving a *name* for the parser, a *start symbol* and *production rules* in the form of
`(A --> B*)`.

### Generating a parser
E.g.:
 
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
      
### Result
The latter macro call will generate the table-driven *top down* parser `grammar-8-parser`, hence the grammar needs to be a *LL(1)* grammar.
`grammar-8-parser` then takes a string and returns a tree .

E.g. 
    
    (grammar-8-parser (list :num #\+ :id))

returns the tree 

    (((((:num)) (#\+ (((:id)))))) :$)
    
You may call it with the keyword argument `:debulk-result t` which will flatten the tree a bit: 

    ((:num (#\+ :id)) :$)

### Logging
For debugging or educational purposes you may use the keyword argument `:stepwise t` which will print the current parser state and ask you, if you want to continue:

    PARSER-TEST> (grammar-8-parser (list :id :$) :stepwise t :debulk-result t)
    TOP: :S
    STACK: (:S :$)
    INPUT: (:ID :$)
    RESULT: ((2))
    Continue? (y or n) y
    TOP: :E
    STACK: (:E :$)
    INPUT: (:ID :$)
    RESULT: ((1) (2))
    Continue? (y or n) y
    TOP: :T
    STACK: (:T :E1 :$)
    INPUT: (:ID :$)
    RESULT: ((2) (1) (2))
    Continue? (y or n) y
    TOP: :F
    STACK: (:F :T1 :E1 :$)
    INPUT: (:ID :$)
    RESULT: ((2) (2) (1) (2))
    Continue? (y or n) y
    TOP: :ID
    STACK: (:ID :T1 :E1 :$)
    INPUT: (:ID :$)
    RESULT: ((1) (2) (2) (1) (2))
    Continue? (y or n) y
    TOP: :T1
    STACK: (:T1 :E1 :$)
    INPUT: (:$)
    RESULT: ((0 :ID) (2) (2) (1) (2))
    Continue? (y or n) y
    TOP: :EPS
    STACK: (:EPS :E1 :$)
    INPUT: (:$)
    RESULT: ((1) (1 (:ID)) (2) (1) (2))
    Continue? (y or n) y
    TOP: :E1
    STACK: (:E1 :$)
    INPUT: (:$)
    RESULT: ((0) (1 (:ID)) (2) (1) (2))
    Continue? (y or n) y
    TOP: :EPS
    STACK: (:EPS :$)
    INPUT: (:$)
    RESULT: ((1) (1 ((:ID))) (1) (2))
    Continue? (y or n) y
    TOP: :$
    STACK: (:$)
    INPUT: (:$)
    RESULT: ((0) (1 ((:ID))) (1) (2))
    Continue? (y or n) y
    
    (:ID :$)

The keyword argument `:log-stream STREAM` will set the stream, to which the log will be written,
if not combined with `:stepwise t` it will print the same log, but doesn't ask, if you want to continue.

### Rejecting
By default, a generated parser *rejects* an input by returning `nil`; the keyword argument `:reject-by-error t` will cause the parser
to throw an error. This *may* give some useful information, of why the parsing failed.

E.g.:
    
    (grammar-8-parser (list :i :$) :reject-by-error t)
 
triggers the debugger

    No entry in the parser table for (:S,:I).
      [Condition of type SIMPLE-ERROR]

    Restarts:
      0: [RETRY] Retry SLIME REPL evaluation request.
      1: [*ABORT] Return to SLIME's top level.
      2: [ABORT] abort thread (#<THREAD "new-repl-thread" RUNNING {1004B05103}>)

    Backtrace:
      0: (GRAMMAR-8-PARSER (:I :|4|) :LOG-STREAM NIL :STEPWISE NIL :REJECT-BY-ERROR T :DEBULK-RESULT NIL)
      1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (GRAMMAR-8-PARSER (LIST :I :|4|) :REJECT-BY-ERROR T) #<NULL-LEXENV>)
      2: (EVAL (GRAMMAR-8-PARSER (LIST :I :|4|) :REJECT-BY-ERROR T))
     --more--

