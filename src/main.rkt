#lang racket

;requirements
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require (lib "eopl.ss" "eopl"))

;lexer
(define lang-lexer
  (lexer
   ("+" (token-plus))
   ("-" (token-minus))
   ("*" (token-mul))
   ("/" (token-div))
   ("**" (token-pow))
   ("<" (token-less))
   (">" (token-greater))
   ("==" (token-equal))
   ("=" (token-assign))
   ("," (token-comma))
   ("[" (token-brack-open))
   ("]" (token-brack-close))
   ("(" (token-paranth-open))
   (")" (token-paranth-close))
   ("True" (token-TRUE))
   ("False" (token-FALSE))
   ("None" (token-NONE))
   ("not" (token-not))
   ("or" (token-or))
   ("and" (token-and))
   (":" (token-colon))
   (";" (token-semicolon))
   ("for" (token-for))
   ("in" (token-in))
   ("break" (token-break))
   ("continue" (token-continue))
   ("if" (token-if)) 
   ("else" (token-else))
   ("def" (token-def))
   ("global" (token-global))
   ("return" (token-return))
   ("pass" (token-pass))
   ("print" (token-print))
   ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme)))
   ((::
     (:or (char-range #\a #\z) (char-range #\A #\Z) #\_)
     (:* (:or (char-range #\a #\z) (char-range #\A #\Z) (char-range #\0 #\9) #\_)))
    (token-ID lexeme))
   (whitespace (lang-lexer input-port))
   ((eof) (token-EOF))))

;tokens
(define-tokens token1 (NUMBER))
(define-tokens token2 (ID))
(define-empty-tokens token3 (
                             EOF
                             plus
                             minus
                             mul
                             div
                             pow
                             less
                             greater
                             equal
                             assign
                             comma
                             brack-open
                             brack-close
                             paranth-open
                             paranth-close
                             TRUE
                             FALSE
                             NONE
                             not
                             or
                             and
                             colon
                             semicolon
                             for
                             in
                             break
                             continue
                             if
                             else
                             def
                             global
                             return
                             pass
                             print
                             ))

;parser
(define lang-parser
  (parser
   (start statements)
   (end EOF)
   (error void)
   (tokens token1 token2 token3)
   (grammar
    (statements
     ((statement semicolon) (statements-exp (list $1)))
     ((statements statement semicolon) (statements-exp (append (exp->statements $1) (list $2)))))
    (statement
     ((compound-stmt) $1)
     ((simple-stmt) $1))
    (simple-stmt
     ((assignment) $1)
     ((return-stmt) $1)
     ((global-stmt) $1)
     ((print-stmt) $1)
     ((pass) (pass-exp))
     ((break) (break-exp))
     ((continue) (continue-exp)))
    (compound-stmt
     ((function-def) $1)
     ((if-stmt) $1)
     ((for-stmt) $1))
    (assignment
     ((ID assign expression) (assignment-exp $1 $3)))
    (return-stmt
     ((return) (return-stmt-exp empty-exp))
     ((return expression) (return-stmt-exp $2)))
    (global-stmt
     ((global ID) (global-stmt-exp $2)))
    (print-stmt
     ((print paranth-open atom paranth-close) (print-stmt-exp $3)))
    (function-def
     ((def ID paranth-open params paranth-close colon statements) (function-def-exp $2 $4 $7))
     ((def ID paranth-open paranth-close colon statements) (function-def-exp $2 (params-exp (list empty-exp)) $6)))
    (params
     ((param-with-default) (params-exp (list $1)))
     ((params comma param-with-default) (params-exp (append (exp->params $1) (list $3)))))
    (param-with-default
     ((ID assign expression) (list $1 $3)))
    (if-stmt
     ((if expression colon statements else-block) (if-stmt-exp $2 $4 $5)))
    (else-block
     ((else colon statements) $3))
    (for-stmt
     ((for ID in expression colon statements) (for-stmt-exp $2 $4 $6)))
    (expression
     ((disjunction) $1))
    (disjunction
     ((conjunction) $1)
     ((disjunction or conjunction) (or-exp $1 $3)))
    (conjunction
     ((inversion) $1)
     ((conjunction and inversion) (and-exp $1 $3)))
    (inversion
     ((not inversion) (not-exp $2))
     ((comparison) $1))
    (comparison
     ((sum compare-op-sum-pairs) (comparison-exp $1 $2))
     ((sum) $1))
    (compare-op-sum-pairs
     ((compare-op-sum-pair) (compare-op-sum-pairs-exp (list $1)))
     ((compare-op-sum-pairs compare-op-sum-pair) (compare-op-sum-pairs-exp (append (exp->compare-op-sum-pairs $1) (list $2)))))
    (compare-op-sum-pair
     ((eq-sum) $1)
     ((lt-sum) $1)
     ((gt-sum) $1))
    (eq-sum
     ((equal sum) (equal-sum-exp $2)))
    (lt-sum
     ((less sum) (less-sum-exp $2)))
    (gt-sum
     ((greater sum) (greater-sum-exp $2)))
    (sum
     ((sum plus term) (add-exp $1 $3))
     ((sum minus term) (sub-exp $1 $3))
     ((term) $1))
    (term
     ((term mul factor) (mul-exp $1 $3))
     ((term div factor) (div-exp $1 $3))
     ((factor) $1))
    (factor
     ((plus factor) (factor-exp "+" $2))
     ((minus factor) (factor-exp "-" $2))
     ((power) $1))
    (power
     ((atom pow factor) (power-exp $1 $3))
     ((primary) $1))
    (primary
     ((atom) $1)
     ((primary brack-open expression brack-close) (list-cell-exp $1 $3))
     ((primary paranth-open paranth-close) (call-exp $1 (arguments-exp null))) ;todo: null or (list empty-exp)?
     ((primary paranth-open arguments paranth-close) (call-exp $1 $3)))
    (arguments
     ((expression) (arguments-exp (list $1)))
     ((arguments comma expression) (arguments-exp (append (exp->arguments $1) (list $3)))))
    (atom
     ((ID) (var-exp $1))
     ((TRUE) (bool-exp #t))
     ((FALSE) (bool-exp #f))
     ((NONE) (none-exp))
     ((NUMBER) (num-exp $1))
     ((list) $1))
    (list
     ((brack-open expressions brack-close) (list-exp $2))
     ((brack-open brack-close) (list-exp null))) ;todo: (list empty-exp) or null?
    (expressions
     ((expressions comma expression) (append $1 (list $3)))
     ((expression) (list $1)))
    )))

;------------------------------------------------------
;evaluate function
(define evaluate
  (lambda (file-name)
    (define ns (make-base-namespace))

    (cond
      [(file-exists? file-name)
       (println (string-append "Test: " file-name))
       (define in (file->string file-name))
       (define lex-this (lambda (lexer input) (lambda () (lexer input))))
       (define my-lexer (lex-this lang-lexer (open-input-string in)))
       (let ((parser-res (lang-parser my-lexer)))
         (initialize-store!)
         (set! the-global-env (init-env #t))
         (set! the-scope-env (init-env #t))
         (value-of parser-res)
         (printf ""))
       ]
      [else (println "File not found")]
      )
   )
 )


;------------------------------------------------------
;print function
;(define (atom? x) (not (pair? x)))

;(define (print inp)
;  (cond
;    [(list? inp) (for-each (lambda (each_atom) (display each_atom) (display " ")) inp) (newline)]
;    [(atom? inp) (display inp)]
;   )
; )

;------------------------------------------------------
;store: Each location may have arbitrary type of value.
(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref)
    (eopl:error 'setref! "Invalid refrence: ~s" ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec
              ((setref-inner
                (lambda (store1 ref1)
                  (cond
                    ((null? store1)
                     (report-invalid-reference ref))
                    ((zero? ref1)
                     (cons val (cdr store1)))
                    (else
                     (cons
                      (car store1)
                      (setref-inner
                       (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

;------------------------------------------------------
;environment: Accepts (string, expval) pairs.
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
     (eopl:error 'apply-env "Bad environment: ~s" env)))
  
(define-datatype environment env?
  (empty-env)
  (extend-env
    (var string?)
    (val expval?)
    (saved-env env?)))

(define apply-env
  (lambda (search-var env with-error)
    (cases environment env
      (empty-env ()
                 (if with-error
                     (report-no-binding-found search-var)
                     (void-val)))
      (extend-env (saved-var saved-val saved-env)
         (if (equal? saved-var search-var)
           saved-val
           (apply-env search-var saved-env with-error)))
      (else
          (report-invalid-env env))
      )))

;doc: Used to initialize the-global-env
(define init-env
  (lambda (global)
          (extend-env "$global" (bool-val global) (empty-env))))

;doc: Used to add defined functions to environment to calling a function.
(define extend-env-with-functions
  (lambda (env)
    (let loop ([env env]
               [g-env the-global-env])
      (cases environment g-env
        (empty-env ()
                   env)
        (extend-env (var val saved-env)
                    (cases expval val
                      (ref-val (ref)
                               (loop (env saved-env)))
                      (proc-val (proc)
                                (loop (extend-env var val env) saved-env))
                      (else
                       (report-type-error)))))
      )))

;doc: Checks whether we are in global scope now
(define global-scope?
  (lambda (env)
    (expval->bool (apply-env "$global" env #t))))

;doc: Used for global variables and functions
(define the-global-env 'uninitialized)

;doc: Used for all known variables in a scope
(define the-scope-env 'uninitialized)

;------------------------------------------------------
;exp
(define-datatype exp exp?
  (empty-exp) ;nothing -> used as epsilon in grammar
  (statements-exp
   (statements-list list?))
  (assignment-exp
   (ID string?)
   (rhs exp?))
  (return-stmt-exp
   (exp1 exp?))
  (global-stmt-exp
   (ID string?))
  (print-stmt-exp
   (atom exp?))
  (pass-exp)
  (break-exp)
  (continue-exp)
  (function-def-exp
   (ID string?)
   (params exp?)
   (statements exp?))
  (if-stmt-exp
   (condition exp?)
   (true-statements exp?)
   (false-statements exp?))
  (for-stmt-exp
   (iterator-ID string?)
   (lst exp?)
   (statements exp?))
  (params-exp
   (exps-list list?))
  ;todo
  ;(param-with-default-exp
  ; (ID string?)
  ; (default exp?))
  (or-exp
   (exp1 exp?)
   (exp2 exp?))
  (and-exp
   (exp1 exp?)
   (exp2 exp?))
  (not-exp
   (exp1 exp?))
  (comparison-exp
   (sum exp?)
   (compare-pairs exp?))
  (compare-op-sum-pairs-exp
   (compare-list list?))
  (equal-sum-exp
   (sum exp?))
  (less-sum-exp
   (sum exp?))
  (greater-sum-exp
   (sum exp?))
  (add-exp
   (exp1 exp?)
   (exp2 exp?))
  (sub-exp
   (exp1 exp?)
   (exp2 exp?))
  (mul-exp
   (exp1 exp?)
   (exp2 exp?))
  (div-exp
   (exp1 exp?)
   (exp2 exp?))
  (factor-exp
   (sign string?)
   (exp1 exp?))
  (power-exp
   (exp1 exp?)
   (exp2 exp?))
  (call-exp
   (proc exp?)
   (arguments exp?))
  (list-cell-exp
   (lst exp?)
   (index exp?))
  (arguments-exp
   (exps-list list?))
  (var-exp
   (name string?))
  (list-exp
   (expressions list?))
  ;const exps
  (num-exp
   (num number?))
  (bool-exp
   (bool boolean?))
  (none-exp)
  )

;exp extractors
(define exp->statements
  (lambda (exp1)
    (cases exp exp1
      (statements-exp (lst) lst)
      (else (report-type-mismatch 'statements-exp exp1)))))

(define exp->params
  (lambda (exp1)
    (cases exp exp1
      (params-exp (lst) lst)
      (else (report-type-mismatch 'params-exp exp1)))))

(define exp->arguments
  (lambda (exp1)
    (cases exp exp1
      (arguments-exp (lst) lst)
      (else (report-type-mismatch 'arguments-exp exp1)))))

(define exp->compare-op-sum-pairs
  (lambda (exp1)
    (cases exp exp1
      (compare-op-sum-pairs-exp (lst) lst)
      (else (report-type-mismatch 'compare-op-sum-pairs-exp exp1)))))

;------------------------------------------------------
;value-of
(define value-of
  (lambda (exp1)
    (cases exp exp1
      (empty-exp () ;nothing -> used as epsilon in grammar
                 (void-val)) 
      (statements-exp (lst)
                      (let loop ([stmt-list lst])
                        (if (null? stmt-list)
                            (void-val)
                            (let ([val1 (value-of (car stmt-list))])
                              (cases expval val1
                                (void-val ()
                                          (loop (cdr stmt-list)))
                                (else val1))))))
      (assignment-exp (ID rhs)
                      (if (global-scope? the-scope-env)
                          (let ([res (apply-env ID the-scope-env #f)]
                                [th (a-thunk rhs the-scope-env)])
                            (cases expval res
                              (void-val ()
                                        (let ([ref (newref th)])
                                          (set! the-global-env (extend-env ID (ref-val ref) the-global-env))
                                          (set! the-scope-env (extend-env ID (ref-val ref) the-scope-env))
                                          (void-val)))
                              (ref-val (ref)
                                       (setref! ref th)
                                       (void-val))
                              (else (report-type-error))))
                          (let ([ref (expval->ref (apply-env ID the-scope-env #t))])
                            (setref! ref (a-thunk rhs the-scope-env))
                            (void-val))))
      (return-stmt-exp (exp1)
                       (return-val (value-of exp1)))
      (global-stmt-exp (ID)
                       (let ([ref (expval->ref (apply-env ID the-global-env))])
                         (set! the-scope-env (extend-env ID (ref-val ref) the-scope-env))
                         (void-val)))
      ;todo: complete print
      (print-stmt-exp (atom)
                      (let ([val (value-of atom)])
                        (cases expval val
                          (num-val (num) (println num))
                          (bool-val (bool) (println bool))
                          (none-val () 33)
                          (list-val (lst) 33)
                          (else (report-type-error)))
                        (void-val)))
      (pass-exp ()
                (void-val))
      (break-exp ()
                 (break-val))
      (continue-exp ()
                    (continue-val))
      (function-def-exp (ID params p-body)
                        (let ([thunk-params
                               (map
                                (lambda (e)
                                  (list (car e) (a-thunk (cadr e) the-scope-env)))
                                (exp->params params))])
                          (set! the-global-env (extend-env ID (proc-val (a-proc ID thunk-params p-body)) the-global-env))
                          (void-val)))
      (if-stmt-exp (exp1 exp2 exp3)
                   (let ([cnd (expval->bool (value-of exp1))])
                     (if cnd
                         (value-of exp2)
                         (value-of exp3))))
      (for-stmt-exp (ID lst for-body)
                    33)
      (params-exp (lst)
                  (report-must-not-reach-here))
      ;todo
      ;(param-with-default-exp
      ; 33)
      (or-exp (exp1 exp2)
              (let ([bool1 (expval->bool (value-of exp1))]
                    [bool2 (expval->bool (value-of exp2))])
                (bool-val (or bool1 bool2))))
      (and-exp (exp1 exp2)
               (let ([bool1 (expval->bool (value-of exp1))]
                     [bool2 (expval->bool (value-of exp2))])
                 (bool-val (and bool1 bool2))))
      (not-exp (exp1)
               (let ([bool1 (expval->bool (value-of exp1))])
                 (bool-val (not bool1))))
      (comparison-exp (sum compare-pairs)
                      33)
      (compare-op-sum-pairs-exp (compare-list)
                                33)
      (equal-sum-exp (sum)
                     33)
      (less-sum-exp (sum)
                    33)
      (greater-sum-exp (sum)
                       33)
      (add-exp (exp1 exp2)
               (let ([val1 (value-of exp1)])
                 (cases expval val1
                   (num-val (num1)
                            (let ([num2 (expval->num (value-of exp2))])
                              (num-val (+ num1 num2))))
                   (bool-val (bool1)
                             (let ([bool2 (expval->bool (value-of exp2))])
                               (bool-val (or bool1 bool2))))
                   (else (report-type-error)))))
      (sub-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1))]
                     [num2 (expval->num (value-of exp2))])
                 (num-val (- num1 num2))))
      (mul-exp (exp1 exp2)
               (let ([val1 (value-of exp1)])
                 (cases expval val1
                   (num-val (num1)
                            (if (zero? num1)
                                (num-val 0)
                                (let ([num2 (expval->num (value-of exp2))])
                                  (num-val (* num1 num2)))))
                   (bool-val (bool1)
                             (if (not bool1)
                                 (bool-val #f)
                                 (let ([bool2 (expval->bool (value-of exp2))])
                                   (bool-val (and bool1 bool2)))))
                   (else (report-type-error)))))
      (div-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1))]
                     [num2 (expval->num (value-of exp2))])
                 (num-val (/ num1 num2))))
      (factor-exp (sign exp1)
                  (let ([num1 (expval->num (value-of exp1))])
                    (if (equal? sign "+")
                        (num-val num1)
                        (num-val (- 0 num1)))))
      (power-exp (exp1 exp2)
                 (let ([num1 (expval->num (value-of exp1))]
                       [num2 (expval->num (value-of exp2))])
                   (num-val (expt num1 num2))))
      (call-exp (exp1 arguments)
                (let ([proc (expval->proc (value-of exp1))]
                      [old-scope-env the-scope-env]
                      [args (exp->arguments arguments)])
                  ;init scope-env for functino call
                  (set! the-scope-env (init-env #f))
                  ;add already defined functions to scope-env
                  (set! the-scope-env (extend-env-with-functions the-scope-env))
                  ;add arguments to scope-env
                  (cases procedure proc
                    (a-proc (ID params p-body)
                            (let loop ([args args]
                                       [params params])
                              (cond
                                [(and (null? params) (null? args)) 88]
                                [(null? params) (report-arguments-len-long)]
                                [(null? args)
                                 (let ([par-with-def (car params)])
                                   (set! the-scope-env (extend-env (car par-with-def) (newref (cadr par-with-def)) the-scope-env))
                                   (loop (args (cdr params))))]
                                [(let ([par-with-def (car params)])
                                   (set! the-scope-env (extend-env (car par-with-def) (newref (a-thunk (car args) the-scope-env)) the-scope-env))
                                   (loop (cdr args) (cdr params)))])))
                    (else
                     (report-type-error)))
                  ;run p-body and return value
                  (cases procedure proc
                    (a-proc (ID params p-body)
                            (let ([ret-val (value-of p-body)])
                              (set! the-scope-env old-scope-env)
                              (cases expval ret-val
                                (void-val () (none-val))
                                (return-val (val) val)
                                (else (report-type-error)))))
                    (else (report-type-error)))))
      (list-cell-exp (lst index)
                     (let ([l (expval->list (value-of lst))]
                           [num (expval->num (value-of index))])
                       (list-ref l num)))
      (arguments-exp (lst)
                     (report-must-not-reach-here))
      (var-exp (var-name)
               (let ([ref (expval->ref (apply-env var-name the-scope-env #t))])
                 (let ([w (deref ref)])
                   (if (expval? w)
                       w
                       (let ([val1 (value-of-thunk w)])
                         (setref! ref val1)
                         val1)))))
      (list-exp (exps-list)
                (list-val (map (lambda (e) (value-of e)) exps-list)))
      ;const exps
      (num-exp (num) (num-val num))
      (bool-exp (bool) (bool-val bool))
      (none-exp () (none-val))
      )))

;------------------------------------------------------
;thunk
(define-datatype thunk thunk?
  (a-thunk
   (exp exp?)
   (saved-env env?)))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp saved-env)
               (let ([old-scope-env the-scope-env])
                 (set! the-scope-env saved-env)
                 (let ([val (value-of exp)])
                   (set! the-scope-env old-scope-env)
                   val))))))

;------------------------------------------------------
;expval
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?))
  (proc-val (proc proc?))
  (void-val)
  (none-val)
  (ref-val (ref number?))
  (break-val)
  (continue-val)
  (return-val(val expval?)))

;expval extractors
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-type-mismatch 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-type-mismatch 'bool val)))))

(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (report-type-mismatch 'list val)))))

(define expval->ref
  (lambda (val)
    (cases expval val
      (ref-val (ref) ref)
      (else (report-type-mismatch 'ref val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-type-mismatch 'proc val)))))


;type error report functions
(define report-type-mismatch
  (lambda (expected val)
    (printf "Type mismatched: Expected ~s but got ~s\n" expected val)))

(define report-type-error
  (lambda ()
    (printf "Type error\n")))

;-------------------------------------------------------
;error report functions
(define report-must-not-reach-here
  (lambda ()
    (println "Must not reach here.")))

(define report-arguments-len-long
  (lambda ()
    (println "Arguments length is too long.")))

;-------------------------------------------------------
;proc
(define-datatype procedure proc?
  (a-proc
   (p-name string?)
   (params exp?)
   (p-body exp?)))

;-------------------------------------------------------
;test: Tests' forlder is "tests"
(define test-dir "../tests/")
(define test-file-name (string-append test-dir "simple-arithmetic_in.txt"))
(evaluate test-file-name)








