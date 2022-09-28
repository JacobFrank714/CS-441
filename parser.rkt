#lang racket

(require "lexer.rkt")
(require parser-tools/lex)

(define (mult_op? x)
    (println "checking mult_op?")
    (cond
        [(eq? (token-name x) 'MULT-OP) #t]
        [else #f]
    )
)

(define (add_op? x)
    (println "checking add_op?")
    (cond   
        [(eq? (token-name x) 'ADD-OP) #t]
        [else #f]
    )
)

(define (factor_tail? x)
    (println "checking factor_tail?")
    (cond
        [(mult_op? (first x))  #t]
        [(factor? (first x)) #t]
        [(factor_tail? (first x)) #t]
        [(eq? (token-name (first x)) 'newline) #t]
        [else #f]
    )
)

(define (factor? x)
    (println "checking factor?")
    (cond
        [(and (eq? (token-name (first x)) 'PAREN-START) (expr? (rest x)) (eq? (token-name (third x)) 'PAREN-END)) #t]
        [(eq? (token-name (first x)) 'IDENTIFIER) #t]
        [(eq? (token-name (first x)) 'NUMBER) #t]
        [else #f]
    )
)

(define (term_tail? x)
    (println "checking term_tail?")
    (cond
        [(add_op? (first x)) #t]
        [(term? (first x)) #t]
        [(term_tail? (first x)) #t]
        [(eq? (token-name (first x)) 'newline) #t]
        [else #f]
    )
)

(define (term? x)
    (println "checking term?")
    (cond 
        [(factor? (first x)) #t]
        [(factor_tail? (first x)) #t]
        [else #f]
    )
)

(define (expr? x)
    (println "checking expr?")
    (cond
        [(term? (first x)) #t]
        [(term_tail? (first x)) #t]
        [else #f]))

(define (stmt? x)
    (println "checking stmt?")
    (cond 
        [(eq? (token-name x) 'IDENTIFIER) #t]
        [(eq? (token-name x) 'READ) #t]
        [(eq? (token-name x) 'WRITE) (expr? (rest x))]
        [else #f]
    )
)

(define (stmt_list? x)
    (println "checking stmt_list?")
    (cond
        [(stmt? (first x)) (stmt_list? (rest x))]
        [(eq? (token-name (first x)) 'newline) #t]
        [else #f]
    )
)

(define (program? x) 
    (println "checking program?")
    (if (stmt_list? x)
        (if(eq? (token-name last x) 'END-OF-PROGRAM) 
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

(define tokens(map position-token-token (string->tokens "input01.txt")))
;;; tokens
(define isProgram?(parse tokens))
isProgram?

;;; gramar on page 107 of book