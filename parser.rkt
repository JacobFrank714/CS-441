#lang racket

(require "lexer.rkt")
(require parser-tools/lex)

(define (id? x)
    (if (eq? (first x) 'IDENTIFIER)
        #t
        #f
    )
)

(define (paren-start? x)
    (if (eq? (first x) 'PAREN-START)
        (expr? (rest x))
        #f
    )
)

(define (mult_op? x)
    (println "checking mult_op?")
    (cond
        [(eq? (token-name (first x)) 'MULT-OP) (factor_tail? (rest x))]
        [else #f]
    )
)

(define (add_op? x)
    (println "checking add_op?")
    (cond   
        [(eq? (token-name (first x)) 'ADD-OP) #t]
        [else #f]
    )
)

(define (factor_tail? x)
    (println "checking factor_tail?")
    (cond
        [(factor? x) (mult_op? (rest x))]
        [(eq? (token-name (first x)) 'newline) #t]
        [else #f]
    )
)


(define (factor? x)
    (println "checking factor?")
    (println (token-name(first x)))
    (cond
        [(eq? (token-name (first x)) 'PAREN-START) (expr? (rest x))]
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
        [(factor? x) (factor_tail? (rest x))]
        [else #f]
    )
)

(define (expr? x)
    (println "checking expr?")
    (cond
        [(term? x) (term_tail? (rest x))]
        [else #f]))

(define (setter? x)
    (println "checking setter?")
    (if (eq? (first x) ':=)
        (expr? (rest x))
        #f
    )
)

(define (stmt? x)
    (println "checking stmt? on")
    (cond 
        [(eq? (token-name (first x)) 'IDENTIFIER) (setter? (rest x))]
        [(eq? (token-name (first x)) 'READ) (id? (rest x))]
        [(eq? (token-name (first x)) 'WRITE) (expr? (rest x))]
        [else #f]
    )
)

(define (stmt_list? x)
    (println "checking stmt_list?")
    (cond
        [(stmt? x) (stmt_list? (rest x))]
        [(eq? (token-name (first x)) 'newline) #t]
        [else #f]
    )
)

(define (program? x) 
    (println "checking program?")
    ;;; (println x)
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