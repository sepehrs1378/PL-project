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
     ((statement semicolon) (list $1))
     ((statements statement semicolon) (append $1 (list $2))))
    (statement
     ((compound-stmt) $1)
     ((simple-stmt) $1))
    (simple-stmt
     ((assignment) $1)
     ((return-stmt) $1)
     ((global-stmt) $1)
     ((pass) $1)
     ((break) $1)
     ((continue) $1))
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
    (function-def
     ((def ID paranth-open params paranth-close colon statements) 33)
     ((def ID paranth-open paranth-close colon statements) 33))
    (params
     ((param-with-default) (list $1))
     ((params comma param-with-default) (append $1 (list $3))))
    (param-with-default
     ((ID assign expression) (param-with-default-exp $1 $3)))
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
     ((sum compare-op-sum-pairs) 33)
     ((sum) $1))
    (compare-op-sum-pairs
     ((compare-op-sum-pair) 33)
     ((compare-op-sum-pairs compare-op-sum-pair) 33))
    (compare-op-sum-pair
     ((eq-sum) 33)
     ((lt-sum) 33)
     ((gt-sum) 33))
    (eq-sum
     ((equal sum) 33))
    (lt-sum
     ((less sum) 33))
    (gt-sum
     ((greater sum) 33))
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
     ((primary paranth-open paranth-close) (call-exp $1 empty-exp))
     ((primary paranth-open arguments paranth-close) (call-exp $1 $3)))
    (arguments
     ((expression) (list $1))
     ((arguments comma expression) (append $1 (list $3))))
    (atom
     ((ID) (var-exp $1))
     ((TRUE) (bool-val #t))
     ((FALSE) (bool-val #f))
     ((NONE) (none-val))
     ((NUMBER) (num-val $1))
     ((list) $1))
    (list
     ((brack-open expressions brack-close) (list-exp $2))
     ((brack-open brack-close) (list-exp (list empty-exp))))
    (expressions
     ((expressions comma expression) (append $1 (list $3)))
     ((expression) (list $1)))
    )))

;------------------------------------------------------
;evalute function
;todo: implement it
(define evalute
  (lambda (file-name)
    ;(define lex-this (lambda (lexer input) (lambda () (lexer input))))
    ;(define my-lexer (lex-this lang-lexer (open-input-string "counter = 2;")))
    ;(let ((parser-res (lang-parser my-lexer))) parser-res)
    33))

;------------------------------------------------------
;print function
;todo: implement it in any way you want

;------------------------------------------------------
;store: todo doc
(define-datatype store store?
  (empty-store)
  (extend-store #|todo: complete this case.|#))

(define init-store
  (lambda ()
    (empty-store)))

;------------------------------------------------------
;environment
(define-datatype env? env
  (empty-env)
  (extend-env
   #|todo: complete this case.|#))

(define init-env
  (lambda ()
    (empty-env)))

;------------------------------------------------------
;exp
(define-datatype exp exp?
  (empty-exp) ;nothing -> used as epsilon in grammar
  (statements-exp
   (other-statements exp?)
   (statment exp?))
  (assignment-exp
   (ID string?)
   (rhs exp?))
  (return-stmt-exp
   (exp1 exp?))
  (global-stmt-exp
   (ID string?))
  (pass-exp)
  (break-exp)
  (continue-exp)
  (function-def-exp
   33)
  (if-stmt-exp
   (condition exp?)
   (true-statements exp?)
   (false-statements exp?))
  (for-stmt-exp
   (iterator-ID string?)
   (lst exp?)
   (statements exp?))
;  (params-exp
;   33)
  (param-with-default-exp
   (ID string?)
   (default exp?))
  (or-exp
   (exp1 exp?)
   (exp2 exp?))
  (and-exp
   (exp1 exp?)
   (exp2 exp?))
  (not-exp
   (exp1 exp?))
  (comparison-exp
   33)
  (compare-op-sum-pairs-exp
   33)
  (compare-op-sum-pair-exp
   33)
  (equal-sum-exp
   33)
  (less-sum-exp
   33)
  (greater-sum-exp
   33)
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
   33)
  (list-cell-exp
   33)
;  (arguments-exp
;   33)
  (var-exp
   (name string?))
  (list-exp
   33)
  )

(define exp->statements
  (lambda (exp)
    (cases exp? exp
      (statements-exp () 33)
      (else 33))))


;------------------------------------------------------
;value-of
(define value-of
  (lambda (exp env)
    (cases exp? exp
      (empty-exp ;nothing -> used as epsilon in grammar
       (void-val)) 
      (statements-exp
       (other-statements exp?)
       (statment exp?))
      (assignment-exp
       33)
      (return-stmt-exp
       33)
      (global-stmt-exp
       33)
      (pass-exp
       (void-val))
      (break-exp
       33)
      (continue-exp
       33)
      (function-def-exp
       33)
      (if-stmt-exp
       33)
      (for-stmt-exp
       33)
      ;  (params-exp
      ;   33)
      (param-with-default-exp
       33)
      (or-exp (exp1 exp2)
              (let ([bool1 (expval->bool (value-of exp1 env))]
                    [bool2 (expval->bool (value-of exp2 env))])
                (bool-val (or bool1 bool2))))
      (and-exp (exp1 exp2)
               (let ([bool1 (expval->bool (value-of exp1 env))]
                     [bool2 (expval->bool (value-of exp2 env))])
                 (bool-val (and bool1 bool2))))
      (not-exp (exp1)
               (let ([bool1 (expval->bool (value-of exp1 env))])
                 (bool-val (not bool1))))
      (comparison-exp
       33)
      (compare-op-sum-pairs-exp
       33)
      (compare-op-sum-pair-exp
       33)
      (equal-sum-exp
       33)
      (less-sum-exp
       33)
      (greater-sum-exp
       33)
      (add-exp (exp1 exp2)
               (let ([let val1 (value-of exp1 env)])
                 (cases expval? val1
                   (num-val (num1)
                            (let ([num2 (expval->num (value-of exp2 env))])
                              (num-val (+ num1 num2))))
                   (bool-val (bool1)
                             (let ([bool2 (expval->bool (value-of exp2 env))])
                               (bool-val (or bool1 bool2))))
                   (else report-type-error))))
      (sub-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1 env))]
                     [num2 (expval->num (value-of exp2 env))])
                 (num-val (- num1 num2))))
      (mul-exp (exp1 exp2)
               (let ([val1 (value-of exp1 env)])
                 (cases expval? val1
                   (num-val (num1)
                            (if (zero? num1)
                                (num-val 0)
                                (let ([num2 (expval->num (value-of exp2 env))])
                                  (num-val (* num1 num2)))))
                   (bool-val (bool1)
                             (if (not bool1)
                                 (bool-val #f)
                                 (let ([bool2 (expval->bool (value-of exp2 env))])
                                   (bool-val (and bool1 bool2)))))
                   (else report-type-error))))
      (div-exp (exp1 exp2)
               (let ([num1 (expval->num (value-of exp1 env))]
                     [num2 (expval->num (value-of exp2 env))])
                 (num-val (/ num1 num2))))
      (factor-exp (sign exp1)
                  (let ([num1 (expval->num (value-of exp1 env))])
                    (if (equal? sign "+")
                        (num-val num1)
                        (num-val (- 0 num1)))))
      (power-exp (exp1 exp2)
                 (let ([num1 (expval->num (value-of exp1 env))]
                       [num2 (expval->num (value-of exp2 env))])
                   (num-val (expt num1 num2))))
      (call-exp
       33)
      (list-cell-exp
       33)
      ;  (arguments-exp
      ;   33)
      (var-exp
       (name string?))
      (list-exp
       33))))

;------------------------------------------------------
;expval
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?))
  (proc-val (proc proc?))
  (void-val)
  (none-val))
;todo: we might need return-val, break-val, continue-val to implement return, break and continue.

(define expval->num
  (lambda (val)
    (cases expval? val
      (num-val (num) num)
      (else report-type-mismatch 'num val))))

(define expval->bool
  (lambda (val)
    (cases expval? val
      (bool-val (bool) bool)
      (else report-type-mismatch 'bool val))))

(define expval->list
  (lambda (val)
    (cases expval? val
      (list-val (lst) lst)
      (else report-type-mismatch 'list val))))

;type error report functions
(define report-type-mismatch
  (lambda (expected val)
    (printf "Type mismatched: Expected ~s but got ~s\n", expected val)))

(define report-type-error
  (lambda ()
    (printf "Type error\n")))


;-------------------------------------------------------
;proc
(define-datatype procedure proc?
  (proc
   (p-name string?)
   (formal-params list?)
   (default-exps list?)
   (p-body exp?)))

;-------------------------------------------------------










