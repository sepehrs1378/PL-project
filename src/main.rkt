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
   ("True" (token-true))
   ("False" (token-false))
   ("None" (token-none))
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
                             true
                             false
                             none
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
     ((statement semicolon) 33)
     ((statements statement semicolon) 33))
    (statement
     ((compound-stmt) 33)
     ((simple-stmt) 33))
    (simple-stmt
     ((assignment) 33)
     ((return-stmt) 33)
     ((global-stmt) 33)
     ((pass) 33)
     ((break) 33)
     ((continue) 33))
    (compound-stmt
     ((function-def) 33)
     ((if-stmt) 33)
     ((for-stmt) 33))
    (assignment
     ((ID assign expression) 33))
    (return-stmt
     ((return) 33)
     ((return expression) 33))
    (global-stmt
     ((global ID) 33))
    (function-def
     ((def ID paranth-open params paranth-close colon statements) 33)
     ((def ID paranth-open paranth-close colon statements) 33))
    (params
     ((param-with-default) 33)
     ((params comma param-with-default) 33))
    (param-with-default
     ((ID assign expression) 33))
    (if-stmt
     ((if expression colon statements else-block) 33))
    (else-block
     ((else colon statements) 33))
    (for-stmt
     ((for ID in expression colon statements) 33))
    (expression
     ((disjunction) 33))
    (disjunction
     ((conjunction) 33)
     ((disjunction or conjunction) 33))
    (conjunction
     ((inversion) 33)
     ((conjunction and inversion) 33))
    (inversion
     ((not inversion) 33)
     ((comparison) 33))
    (comparison
     ((sum compare-op-sum-pairs) 33)
     ((sum) 33))
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
     ((sum plus term) 33)
     ((sum minus term) 33)
     ((term) 33))
    (term
     ((term mul factor) 33)
     ((term div factor) 33)
     ((factor) 33))
    (factor
     ((plus factor) 33)
     ((minus factor) 33)
     ((power) 33))
    (power
     ((atom pow factor) 33)
     ((primary) 33))
    (primary
     ((atom) 33)
     ((primary brack-open expression brack-close) 33)
     ((primary paranth-open paranth-close) 33)
     ((primary paranth-open arguments paranth-close) 33))
    (arguments
     ((expression) 33)
     ((arguments comma expression) 33))
    (atom
     ((ID) 33)
     ((true) 33)
     ((false) 33)
     ((none) 33)
     ((NUMBER) 33)
     ((list) 33))
    (list
     ((brack-open expressions brack-close) 33)
     ((brack-open brack-close) 33))
    (expressions
     ((expressions comma expression) 33)
     ((expression) 33))
    )))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this lang-lexer (open-input-string "counter = 2;")))
(let ((parser-res (lang-parser my-lexer))) parser-res)