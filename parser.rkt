#lang racket

(require "lexer.rkt")
(require parser-tools/lex)

(define (mult_op? x)
    (cond
        [(= x "*") #t]
        [(= x "/") #t]
        [else #f]
    )
)

(define (add_op? x)
    (cond   
        [(= x "+") #t]
        [(= x "-") #t]
        [else #f]
    )
)

(define (factor_tail? x)
    (cond
        [(mult_op? (x))  #t]
        [(factor? (x)) #t]
        [(factor_tail? (x)) #t]
        [("") #t]
        [else #f]
    )
)

(define (factor? x)
    (cond
        [(= x "(") #t]
        [(expr? (x)) #t]
        [(= x ")") #t]
        [else #f]
    )
)

(define (term_tail? x)
    (cond
        [((add_op? (x))) #t]
        [(term? (x)) #t]
        [(term_tail? (x)) #t]
        [(= x "") #t]
        [else #f]
    )
)

(define (term? x)
    (cond 
        [(factor? (x)) #t]
        [(factor_tail? (x)) #t]
        [else #f]
    )
)

(define (expr? x)
    (cond
        [(term? (x)) #t]
        [(term_tail? (x)) #t]
        [else #f]))

(define (stmt? x)
    (cond 
        [(= x "id") #t]
        [(= x "read") #t]
        [(= x "write") (expr?(x))]
        [else #f]
    )
)

(define (stmt_list? x)
    (cond
        [(stmt?(x)) #t]
        [(stmt_list?(x)) #t]
        [(= x "") #t]
        [else #f]
    )
)

(define (program? x) 
    (if (stmt_list?(x))
        (if(= x "$$") 
            #t 
            #f
        ) 
        #f
    )
)

(define (parse tokens)
    (define prog? (program? tokens))
    prog?
)

(define tokens(map position-token-token (string->tokens "input02.txt")))
tokens
;;; (define isProgram?(parse tokens))
;;; isProgram?